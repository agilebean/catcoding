################################################################################
#
# Script:  encoders/scikit-encoders.R
# Output:  training.set, testing.set - library: category_encoders
#
################################################################################
import category_encoders as ce
import os
os.environ["OMP_NUM_THREADS"] = "1"

def get_scikit_encoder(encoding, cat_labels):
  
  if (encoding == "scikit-backward"):
  
    encoding_function = ce.BackwardDifferenceEncoder
    
  elif (encoding == "scikit-baseN"):
    
    encoding_function = ce.BaseNEncoder

  elif (encoding == "scikit-binary"):
    
    encoding_function = ce.BinaryEncoder
    
  elif (encoding == "scikit-catboost"):
    
    encoding_function = ce.CatBoostEncoder

  elif (encoding == "scikit-glmm"):
    
    encoding_function = ce.GLMMEncoder

  elif (encoding == "scikit-hashing"):
    
    encoding_function = ce.HashingEncoder

  elif (encoding == "scikit-helmert"):

    encoding_function = ce.HelmertEncoder

  elif (encoding == "scikit-james-stein"):

    encoding_function = ce.JamesSteinEncoder

  elif (encoding == "scikit-loo"):
    
    encoding_function = ce.LeaveOneOutEncoder
    
  elif (encoding == "scikit-Mestimate"):
    
    encoding_function = ce.MEstimateEncoder

  elif (encoding == "scikit-onehot"):

    encoding_function = ce.OneHotEncoder

  elif (encoding == "scikit-ordinal"):
  
    encoding_function = ce.OrdinalEncoder

  elif (encoding == "scikit-polynomial"):

    encoding_function = ce.PolynomialEncoder

  elif (encoding == "scikit-sum"):
    
    encoding_function = ce.SumEncoder
    
  elif (encoding == "scikit-target"):
    
    encoding_function = ce.TargetEncoder

  elif (encoding == "scikit-woe"):

    encoding_function = ce.WOEEncoder

  
  # pass cols specification & drop zero-variance vars
  encoding_function = encoding_function(cols = cat_labels, drop_invariant = True)
  
  return(encoding_function)
