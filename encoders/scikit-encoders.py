################################################################################
#
# Script:  encoders/scikit-encoders.R
# Output:  training.set, testing.set - library: category_encoders
#
################################################################################
import category_encoders as ce

def get_scikit_encoder(encoding, cat_labels):
  
  if (encoding == "scikit-target"):
    
    encoding_function = ce.TargetEncoder
  
  elif (encoding == "scikit-ordinal"):
  
    encoding_function = ce.OrdinalEncoder
  
  elif (encoding == "scikit-backward"):
  
    encoding_function = ce.BackwardDifferenceEncoder
  
  elif (encoding == "scikit-helmert"):
  
    encoding_function = ce.HelmertEncoder

  elif (encoding == "scikit-helmert"):

    encoding_function = ce.HelmertEncoder
  
  elif (encoding == "scikit-james-stein"):

    encoding_function = ce.JamesSteinEncoder

  elif (encoding == "scikit-polynomial"):

    encoding_function = ce.PolynomialEncoder

  elif (encoding == "scikit-woe"):

    encoding_function = ce.WOEEncoder

  elif (encoding == "scikit-binary"):

    encoding_function = ce.BinaryEncoder
  
  elif (encoding == "scikit-onehot"):

    encoding_function = ce.OneHotEncoder
  
  # pass cols specification & drop zero-variance vars
  encoding_function = encoding_function(cols = cat_labels, drop_invariant = True)
  
  return(encoding_function)
