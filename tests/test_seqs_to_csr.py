# test_collision.py
import unittest
import pytest
from tcrtest.nx import seqs_to_csr
from tcrtest.collision import deletion, collision, collision2
import numpy as np 

class TestSeqsToCSR(unittest.TestCase):
    def setUp(self):
        self.seqs2 = ['CASF',
                      'XASF',
                      'CSAF']
        self.seqs = ['CASSVIRSSYEQYF',
                     'CASSVIRSSYEQYF',
                     'CASSIRSSYEAFF',
                     'CASSIRSSWEQYF',
                     'CASSIRSSPEQFF',
                     'CATSIRSSYEQYF',
                     'CASSIRSSLEQYF',
                     'CASSIRSSLEQFF',
                     'CASSNIRSSYEQYF']
        self.gliphs = ['VI.SSYEQYF','VI.SSYEQYF','VIR.SYEQYF','VIXSSYEQYF']


    def test_w_seqs(self):
        # collision is the defeault collision func (edit-1 - i.e. indel or sub)
        S0, S1 = seqs_to_csr(self.seqs, cpus = 1, collision = collision)
        #import pdb; pdb.set_trace()
        S0ex = np.array([
        [1, 1, 0, 0, 0, 0, 0, 0, 0],
        [1, 1, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 1, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 1, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 1, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 1, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 1]], dtype=np.int8)
        assert np.all(S0.todense() == S0ex)
        S1ex = np.array([
        [1, 1, 0, 0, 0, 0, 0, 0, 1],
        [1, 1, 0, 0, 0, 0, 0, 0, 1],
        [0, 0, 1, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 1, 0, 0, 1, 0, 0],
        [0, 0, 0, 0, 1, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 1, 0, 0, 0],
        [0, 0, 0, 1, 0, 0, 1, 1, 0],
        [0, 0, 0, 0, 1, 0, 1, 1, 0],
        [1, 1, 0, 0, 0, 0, 0, 0, 1]], dtype=np.int8)
        assert np.all(S1.todense() == S1ex)

    # this illustrates how another collision can be provided
    def test_w_seqs_symdel(self):
        S0, S1 = seqs_to_csr(self.seqs2, cpus = 1, collision = deletion)
        S1ex = np.array([[1, 1, 1],
                         [1, 1, 0],
                         [1, 0, 1]], dtype=np.int8)
        S0ex = np.array([[1, 0, 0],
                         [0, 1, 0],
                         [0, 0, 1]], dtype=np.int8)
        assert np.all(S1.todense() == S1ex)
        assert np.all(S0.todense() == S0ex)
        #import pdb; pdb.set_trace()

    def test_w_seqs_collision2(self):
        S0, S1 = seqs_to_csr(self.seqs2, cpus = 1, collision = collision2)
        S1ex = np.array([[1, 1, 1],
                         [1, 1, 0],
                         [1, 0, 1]], dtype=np.int8)
        S0ex = np.array([[1, 0, 0],
                         [0, 1, 0],
                         [0, 0, 1]], dtype=np.int8)
        assert np.all(S1.todense() == S1ex)
        assert np.all(S0.todense() == S0ex)
        #import pdb; pdb.set_trace()

    def test_w_gliphs(self):
        # illustrates that gliphs will actually colide with each other in edit-2 fasshon
        S0, S1 = seqs_to_csr(self.gliphs, cpus = 1)
        #import pdb; pdb.set_trace()

        S0ex = np.array([
        [1, 1, 0, 0],
        [1, 1, 0, 0],
        [0, 0, 1, 0],
        [0, 0, 0, 1]], dtype=np.int8)
        assert np.all(S0.todense() == S0ex)
        S1ex = np.array([
        [1, 1, 1, 1],
        [1, 1, 1, 1],
        [1, 1, 1, 0],
        [1, 1, 0, 1]], dtype=np.int8)
        assert np.all(S1.todense() == S1ex)


if __name__ == '__main__':
    unittest.main()


