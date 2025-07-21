import numpy as np 
import os
import pandas as pd 
import parmap 
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegressionCV
from sklearn.metrics import f1_score, roc_auc_score, confusion_matrix
from sklearn.metrics import roc_curve, roc_auc_score
from sklearn.utils.class_weight import compute_class_weight
import copy
import scipy

class HLApredict:
    def __init__(self, Q, X=None, Y=None, cpus = 2):
        self.cpus = cpus
        self.Q = Q 
        self.X = X
        self.Y = Y 
        self.X_train = None
        self.X_test  = None
        self.y_train = None
        self.y_test  = None
    def copy(self):
        return copy.deepcopy(self)

    def test_train_split_data_cols_X_rows_Y(self, X=None, Y=None, train_size=250, test_size=250, random_state=1):
        """
        Split the columns of the DataFrame into train and test sets without overlap.
        
        :param data: The DataFrame from which to sample columns.
        :param train_size: The number of columns to include in the train set.
        :param test_size: The number of columns to include in the test set.
        :param random_state: The random seed for reproducibility.
        :return: Two DataFrames: train_data (with sampled train columns), test_data (with sampled test columns).
        """
        
        if X is None:
            X = self.X
        if Y is None:
            Y = self.Y
        assert train_size + test_size <= X.shape[1]
        
        assert X.shape[1] == Y.shape[0]
        assert  len(Y.index.difference(X.columns)) == 0

        np.random.seed(random_state)
        train_columns = np.random.choice(X.columns, size=train_size, replace=False)
        
        remaining_columns = X.columns.difference(train_columns)
        np.random.seed(random_state)  # Optional: reset the seed for test data if desired
        test_columns = np.random.choice(remaining_columns, size=test_size, replace=False)
        
        X_train = X[train_columns]
        X_test  = X[test_columns]
        y_train = Y.loc[train_columns] 
        y_test  = Y.loc[test_columns]
        self.X_train = X_train
        self.X_test  = X_test
        self.y_train = y_train
        self.y_test  = y_test
    

    def load_fit(self, model_folder, model_name, use_npz = True):
        if use_npz:
            df = load_weights_from_npz(
                    weights_npz=os.path.join(model_folder, f'{model_name}.weights.npz'), #'XSTUDY_ALL_FEATURE_L1_v4.weights.npz', 
                    weights_col=os.path.join(model_folder, f'{model_name}.columns.csv')) #'XSTUDY_ALL_FEATURE_L1_v4.columns.csv')
            self.coefficients   = df
            self.intercepts     = pd.read_csv(os.path.join(model_folder, f'{model_name}.intercepts.csv'), index_col = 0)

        else:
            self.coefficients   = pd.read_csv(os.path.join(model_folder, f'{model_name}.weights.csv')   , index_col = 0)
            self.intercepts     = pd.read_csv(os.path.join(model_folder, f'{model_name}.intercepts.csv'), index_col = 0)
    
    def load_data(self, model_folder, model_name):
        self.X = pd.read_csv(os.path.join(model_folder, f'{model_name}.training_data.csv'), index_col = 0)
        self.Y = pd.read_csv(os.path.join(model_folder, f'{model_name}.observations.csv'), index_col = 0)
    
    def load_calibrations(self, model_folder, model_name):
        self.calibrations = pd.read_csv(os.path.join(model_folder, f'{model_name}.calibrations.csv'), index_col = 0)#.to_dict()

        
    def fit(self, variables = None, penalty = "l2", model_func= None):
        """

        """
        if variables is None:
            variables =  self.Q.binary.value_counts().index
        if model_func is None:
            model_func = fit_weights_and_intercept

        rx = parmap.map(model_func, 
                variables,
                Q=self.Q, 
                X01 = self.X_train, 
                Y = self.y_train, 
                penalty = penalty, 
                pm_pbar = True, 
                pm_processes=self.cpus)
        
        coefficients = pd.concat({x[0]:x[1] for x in rx}, axis = 1)
        coefficients = coefficients.reindex(self.Q.index).fillna(0).sort_index()

        self.coefficients = coefficients
        self.intercepts   = pd.DataFrame.from_dict({x[0]:x[2] for x in rx}, orient='index', columns = ['intercept'])

    def save_fit(self, model_folder, model_name):
        from scipy.sparse import csr_matrix, save_npz
        print(f"SAVING {model_folder}/{model_name}.weights.npz")
        print(f"SAVING {model_folder}/{model_name}.columns.csv")
        print(f"SAVING {model_folder}/{model_name}.intercepts.csv")
        ws = csr_matrix(self.coefficients.values)
        save_npz(os.path.join(model_folder, f"{model_name}.weights.npz"), ws)
        pd.DataFrame(self.coefficients.columns).\
            to_csv(os.path.join(model_folder, f"{model_name}.columns.csv"), index = False)
        self.intercepts.to_csv(os.path.join(model_folder, f"{model_name}.intercepts.csv"))

    def save_calibration(self, model_folder, model_name):
        pd.DataFrame(self.calibrations).to_csv(os.path.join(model_folder,f"{model_name}.calibrations.csv"))

    def predict_decisions_x(self, X, C = None):
        # X is a transpose
        #X = (X > 0).astype(int).values.transpose()
        #if C is not None:
        #    X = pd.concat([X, C])
        assert X.values.dtype == 'int64', "Xt values are not of type int64"
        W = self.coefficients
        i = self.intercepts
        binary_variables = W.columns
        ic_ = np.array(i['intercept'].to_list())
        ic_tile = np.tile(ic_, (X.shape[0], 1))
        z = np.dot(X,W)

        decision_scores = pd.DataFrame(z, columns = binary_variables)
        
        decision_scores.index = X.index
        
        self.decision_scores = decision_scores

        return decision_scores 

    # def predict_decisions(self, X, C = None):
    #     X = (X > 0).astype(int).values.transpose()
    #     if C is not None:
    #         X = pd.concat([X, C])
    #     W = self.coefficients
    #     i = self.intercepts
    #     binary_variables = W.columns
    #     ic_ = np.array(i['intercept'].to_list())
    #     ic_tile = np.tile(ic_, (X.shape[0], 1))
    #     z = np.dot(X,W)

    #     decision_scores = pd.DataFrame(z, columns = binary_variables )
    #     self.decision_scores = decision_scores
    #     return decision_scores 

    # def set_calibrations(self,decision_scores=None,observations=None, variables = None):
    #     import statsmodels.api as sm
    #     if decision_scores is None:
    #         decision_scores = self.decision_scores
    #     if observations is None:
    #         observations = self.y_test

    #     calibrations = dict()
    #     if variables is None:
    #         variables =  sorted(self.Q.binary.unique())
    #     for binary in variables:
    #         print(binary)
    #         d = pd.DataFrame({'p':decision_scores[binary].values,'obs':observations[binary].values}, 
    #             index = observations.index )
    #         d = d.dropna()
    #         #platt_model = LogisticRegression(C=1e10)
    #         #decision_scores_reshaped = d['p'].values.reshape(-1, 1)
    #         #platt_model.fit(decision_scores_reshaped, d['obs'])
    #         y = d['obs']   
    #         Xk = sm.add_constant(d[['p']])
    #         model = sm.Logit(y, Xk)
    #         result = model.fit()
    #         calib0 = result.params.iloc[0]
    #         calib1 = result.params.iloc[1]
    #         calibrations[binary] = {'intercept':result.params.iloc[0], 'coef':result.params.iloc[1]}
    #     self.calibrations = calibrations
    #     return calibrations

    # def predict_calibrated_prob(self, decision_scores, variables = None):
    #     calibrated_pred = list()
    #     if variables is None:
    #         variables = sorted(self.Q.binary.unique())
    #     for binary in variables:
    #         print(binary)
    #         intercept = self.calibrations[binary]['intercept']
    #         coef      = self.calibrations[binary]['coef']
    #         Xb = decision_scores[binary].values
    #         z = -1*(intercept + np.dot(Xb, coef))
    #         prob = (1 / (1 + np.exp(z)))
    #         calibrated_pred.append(pd.DataFrame({binary:prob}))

    #     calibrated_prob = pd.concat(calibrated_pred, axis =1)
    #     self.calibrated_prob = calibrated_prob
    #     return calibrated_prob

    def score_predictions(self, probs, observations, thr = .5, gate =(.5,.5), variables = None):
        from sklearn.metrics import f1_score, roc_auc_score, confusion_matrix
        from sklearn.metrics import roc_curve, roc_auc_score
        perform= list()
        if variables is None:
            variables = sorted(self.Q.binary.unique())
        for binary in variables:
            #print(binary)
            try:
                if binary in observations.columns:
                    d = pd.DataFrame({'p':probs[binary].values,'obs':observations[binary].values}, index = observations.index )
                    d = d.dropna().sort_values('p')
                    gate_ix = (d['p'] <= gate[0])| (d['p'] >= gate[1])
                    n_pre = d.shape[0]
                    d = d[gate_ix]
                    n_post = d.shape[0]
                    y_pred =  d['p'] > thr
                    y_test =  d['obs'].astype('bool')
                    auc = roc_auc_score(y_test, d['p'] )
                    f1 = f1_score(y_test, y_pred)
                    if y_pred.sum() > 0:
                        tn, fp, fn, tp = confusion_matrix(y_test, y_pred).ravel()
                        sens = tp / (tp + fn)
                        spec = tn / (tn + fp)
                        ba_acc = (sens + spec) / 2
                        acc = (tp+tn)/(tp+fp+fn+tn)
                        mse = np.sum((y_pred-y_test.astype('float64'))**2) / y_pred.shape[0]
                        perform.append((binary,tn,fp,fn,tp,sens,spec,ba_acc,acc,auc,mse,f1,n_pre,n_post,gate[0],gate[1]))
            except ValueError:
                #print(f"XXXXX {binary} XXXXX")
                continue
        perform_df = pd.DataFrame(perform, columns = 'binary,tn,fp,fn,tp,sens,spec,ba_acc,acc,auc,mse,f1,n_total,n_gated,gate1,gate2'.split(','))
        return perform_df



    def predict_raw_prob(self, X, C = None):
        X = (X > 0).astype(int).values.transpose()
        if C is not None:
            X = pd.concat([X, C])
        W = self.coefficients
        i = self.intercepts
        binary_variables = W.columns
        ic_ = np.array(i['intercept'].to_list())
        ic_tile = np.tile(ic_, (X.shape[0], 1))
        z = np.dot(X,W) + ic_tile
        P_prob = 1 / (1 + np.exp(-z))
        P_df = pd.DataFrame(P_prob, columns = binary_variables )
        return P_df

    def output_probs_and_obs(self, probs, observations, variables = None):
        store = list()
        if variables == None:
            variables =sorted(self.Q.binary.unique())
        for binary in variables:
            if binary in observations.columns:
                d = pd.DataFrame({'p':probs[binary].values,'obs':observations[binary].values}, index = observations.index )
                d['sample_id'] = observations.index
                d['binary'] = binary
                store.append(d)
        return pd.concat(store).reset_index(drop = True)
    
    def scx(self,decision_scores=None, observations=None, variables=None, covariates=None):
        """
        Set calibration parameters for decision scores with optional additional variables.

        Args:
           decision_scores (pd.DataFrame): DataFrame of decision scores for each binary classifier.
           observations (pd.DataFrame): DataFrame of observed binary outcomes (0 or 1).
           variables (list): List of binary labels to calibrate. Defaults to all in self.Q.binary.unique().
           additional_vars (pd.DataFrame): Optional DataFrame of additional variables (features).

        Returns:
           dict: Calibration parameters for each binary label.
        """
        import statsmodels.api as sm

        if decision_scores is None:
           decision_scores = self.decision_scores
        if observations is None:
           observations = self.y_test
       
        if variables is None:
           variables = sorted(self.Q.binary.unique())
        calibrations = dict()
        for binary in variables:
            print(binary)

            # Create a DataFrame with decision scores and observations
            d = pd.DataFrame({
             'p': decision_scores[binary].values,
             'obs': observations[binary].values
            }) 
            d.index=observations.index

            # Include additional variables if provided
            if covariates is not None:
                for col in covariates.columns:
                    d[col] = covariates[col].astype('float')

            # Drop rows with missing values
            d = d.dropna().astype(float)

            # Prepare response variable and features
            y = d['obs']
            Xk = sm.add_constant(d.drop(columns=['obs']))  # Include decision scores and additional variables
            from numpy.linalg import LinAlgError
            # Fit logistic regression model
            try: 
                model = sm.Logit( y.values,Xk.values)
                result = model.fit()
                print(result.summary())

            except LinAlgError as e:
                print(f"LinAlgError encountered: {e}")
                #import pdb; pdb.set_trace()
                try:
                    model = sm.Logit( y.values,Xk.values)
                    result = model.fit_regularized()
                    print(result.summary())
                except:
                    continue

            # Extract coefficients
            params = pd.Series(result.params)
            intercept = params.iloc[0]
            coef = params.iloc[1:]  # Coefficients for decision scores and additional variables

            # Store calibration parameters
            calibrations[binary] = {
                'intercept': intercept,
                'coef': coef.values[0:],
                'coef_vars' : Xk.columns[2:]}

        #import pdb; pdb.set_trace()
        new_i = pd.DataFrame({k:[x['intercept']] for k,x, in calibrations.items()}, index =["intercept"])
        new_c = {k: np.array([np.nan]) if len(x['coef']) == 0 else x['coef'] for k, x in calibrations.items()}
        new_c = pd.DataFrame(new_c)
        if covariates is not None:
            new_c.index = ['coef'] + covariates.columns.to_list()
        else:
            new_c.index = ['coef']
        new = pd.concat([new_i,new_c])
        #new.to_csv(os.path.join(model_folder, f"{model_name}.calibrations_v625_x.csv"))
        #new = pd.read_csv(os.path.join(model_folder, f'{model_name}.calibrations_v625_x.csv'), index_col = 0).to_dict()
        self.calibrations = new
        return new

    def pxx(self, decision_scores, variables=None, covariates=None):
        calibrated_pred = {}
        if variables is None:
            variables = sorted(self.Q.binary.unique())

        if covariates is not None:
            assert isinstance(covariates, pd.DataFrame), "Covariates must be a pandas DataFrame."
            assert decision_scores.shape[0] == covariates.shape[0], \
            "Decision scores and covariates must have the same number of rows."
            assert np.all(decision_scores.index == covariates.index)

         # Calculate calibrated probabilities for each binary variable
        for binary in variables:
            if binary in self.calibrations.keys():
                #print(f"Calibrating for binary variable: {binary}")
                # Retrieve intercept and coefficients
                intercept = self.calibrations[binary]['intercept']
                # Retrieve coeficients
                if covariates is not None:
                        all_variables =  ['coef'] + covariates.columns.to_list()
                else:
                    all_variables = ['coef']
                coef = np.array([self.calibrations[binary][col] for col in all_variables])

                # Combine decision scores and covariates if applicable
                if covariates is None:
                    Xb = decision_scores[[binary]].values
                else:
                    Xb = np.hstack([decision_scores[[binary]].values, covariates.values])

            # Validate the shapes match
            assert Xb.shape[1] == coef.shape[0], \
            f"Shape mismatch: Xb has {Xb.shape[1]} features, but coef expects {coef.shape[0]}."
            # Compute probabilities using the logistic function
            Xb = Xb.astype(float)
            z = -1 * (intercept + np.dot(Xb, coef))
            #import pdb;pdb.set_trace()
            z=z.astype(float)
            prob = 1 / (1 + np.exp(z))
            # Store the probabilities
            calibrated_pred[binary] = prob

        # Create a DataFrame from the results
        calibrated_prob = pd.DataFrame(calibrated_pred, index = decision_scores.index)
        
        self.calibrated_prob = calibrated_prob
        return calibrated_prob


    def predict_calibrated_prob2(self, decision_scores, variables=None, covariates=None):
       """
       Predict calibrated probabilities based on decision scores.

       Parameters:
       decision_scores : pd.DataFrame
           DataFrame containing decision scores for each binary variable.
       variables : list, optional
           List of binary variables to calibrate. If None, uses all binary variables in self.Q.
       covariates : pd.DataFrame or None, optional
           Additional covariates to include in the calibration. If None, only decision_scores are used.

       Returns:
       pd.DataFrame
           DataFrame with calibrated probabilities for each binary variable.
       """
       calibrated_pred = {}

       # Default to all binary variables if none are specified
       if variables is None:
           variables = sorted(self.Q.binary.unique())

       # Validate input shapes if covariates are provided
       if covariates is not None:
            assert isinstance(covariates, pd.DataFrame), "Covariates must be a pandas DataFrame."
            assert decision_scores.shape[0] == covariates.shape[0], \
               "Decision scores and covariates must have the same number of rows."
            assert np.all(decision_scores.index == covariates.index)
       # Calculate calibrated probabilities for each binary variable
       for binary in variables:
           if binary in self.calibrations.keys():
                print(f"Calibrating for binary variable: {binary}")

                # Retrieve intercept and coefficients
                intercept = self.calibrations[binary]['intercept']
                coef = np.array(self.calibrations[binary]['coef'])

                # Combine decision scores and covariates if applicable
                if covariates is None:
                    Xb = decision_scores[[binary]].values
                else:
                    Xb = np.hstack([decision_scores[[binary]].values, covariates[self.calibrations[binary]['coef_vars']].values])

                # Validate the shapes match
                assert Xb.shape[1] == coef.shape[0], \
                    f"Shape mismatch: Xb has {Xb.shape[1]} features, but coef expects {coef.shape[0]}."

                # Compute probabilities using the logistic function

                Xb = Xb.astype(float)
                z = -1 * (intercept + np.dot(Xb, coef))
                #import pdb;pdb.set_trace()
                z=z.astype(float)
                prob = 1 / (1 + np.exp(z))

                # Store the probabilities
                calibrated_pred[binary] = prob

       # Create a DataFrame from the results
       calibrated_prob = pd.DataFrame(calibrated_pred)
       
       return calibrated_prob



import re
def map_allele(allele):
    # Define the sets of prefixes to categorize
    categories = {
        'A': 'A',
        'B': 'B',
        'C': 'C',
        'DPA': 'DP',
        'DPB': 'DP',
        'DQA': 'DQ',
        'DQB': 'DQ',
        'DRB': 'DR'
    }
    
    # Function to map each allele to its category
    def get_category(allele):
        # Check for DPAB and DQAB combinations using regex patterns
        if re.match(r'DQA1_\d+__DQB1_\d+', allele):
            return 'DQAB'
        elif re.match(r'DPA1_\d+__DPB1_\d+', allele):
            return 'DPAB'
        
        # Check for individual allele types
        for key in categories:
            if allele.startswith(key):
                return categories[key]
        
        return None  # Return None if no category found
    
    return get_category(allele)
    
import re
def map_allele2(allele, resolve_DR = True):
    # Define the sets of prefixes to categorize
    categories = {
        'A': 'A',
        'B': 'B',
        'C': 'C',
        'DPA': 'DPA',
        'DPB': 'DPB',
        'DQA': 'DQA',
        'DQB': 'DQB',
        'DRB': 'DR'
    }

    def get_category(allele):
        # Check for DPAB and DQAB combinations using regex patterns
        if re.match(r'DQA1_\d+__DQB1_\d+', allele):
            return 'DQAB'
        elif re.match(r'DPA1_\d+__DPB1_\d+', allele):
            return 'DPAB'
        
        # Check for individual allele types
        for key in categories:
            if allele.startswith(key):
                return categories[key]
                
    if resolve_DR and re.match(r'DRB', allele):
        if allele.startswith('DRB3'):
            return 'DR3'
        elif allele.startswith('DRB4'):
            return 'DR4'
        elif allele.startswith('DRB5'):
            return 'DR5'
        elif allele.startswith('DRB1'):
            return get_category(allele)
    else:
        return get_category(allele)




def fit_weights_and_intercept(binary,Q, X01,Y,penalty = "l2"):
    try:
       print(binary)
       binary_ix = Q[Q['binary'] == binary].index
       Xq = X01.iloc[binary_ix,:]

       # Subset the labels to binary column
       yq = Y[binary]
       assert (yq.index == Xq.columns).all()

       # Address Samples with NAs
       valid_samples = yq[yq.notna()].index
       Xq = Xq[valid_samples]
       yq = yq[yq.notna()]
       assert (yq.index == Xq.columns).all()
       # Make binary
       Xq = Xq.values.transpose()
       yq = yq.astype(int).values

       class_weights = compute_class_weight(class_weight='balanced', classes=np.unique(yq), y=yq)
       class_weight_dict = {i: class_weights[i] for i in range(len(class_weights))}
       logreg_cv = LogisticRegressionCV(max_iter=2000,cv=5, class_weight=class_weight_dict, 
         scoring='roc_auc', penalty = penalty, solver = 'liblinear', random_state = 1)
       logreg_cv.fit(Xq, yq)
       coefs = logreg_cv.coef_
       intercept = logreg_cv.intercept_[0]
       coefs = pd.Series(coefs[0], index = binary_ix)
       return(binary, coefs, intercept, None)
    except KeyError:
       #import pdb; pdb.set_trace()
       print(binary)
       return(binary, None, None,None)

def load_weights_from_npz(
    weights_npz='XSTUDY_ALL_FEATURE_L1_v4.weights.npz', 
    weights_col='XSTUDY_ALL_FEATURE_L1_v4.columns.csv'):
    S = scipy.sparse.load_npz(weights_npz)
    w_cols = pd.read_csv(weights_col).iloc[:,0].to_list()
    df = pd.DataFrame(
        S.toarray(), 
        columns = w_cols )
    return(df)