import pytest
import os
from tcrtest.hello import main

def test_hello_main_returns_exepcted_models():
    subfolders = main()
    print(subfolders)
    model_names = [os.path.basename(x) for x in subfolders]
    assert 'XSTUDY_ALL_FEATURE_L1_v4e' in model_names
    # Add more assert statments as additioanl models are added
