import category_encoders as ce
# import sys
# 
# ENCODING = sys.argv[1]
# CAT_labels = sys.argv[2]
# 
# print(ENCODING)
# print(CAT_labels)

def apply_scikit_encoder(encoding, cat_labels):
  
  if (encoding == "scikit-target"):
    
    encoding_function = ce.TargetEncoder
  
  elif (encoding == "scikit-ordinal"):
  
    encoding_function = ce.OrdinalEncoder
  
  elif (encoding == "scikit-backward-difference"):
  
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

# def apply_scikit_encoder(encoding, cat_labels):
#   
#   if (encoding == "scikit-target"):
#     
#     encoder = ce.TargetEncoder(cols = cat_labels, drop_invariant = True)
#   
#   elif (encoding == "scikit-ordinal"):
#   
#     encoder = ce.OrdinalEncoder(cols = cat_labels, drop_invariant = True)
#   
#   elif (encoding == "scikit-backward-difference"):
#   
#     encoder = ce.BackwardDifferenceEncoder(
#       cols = cat_labels, drop_invariant = True
#     )
#   
#   elif (encoding == "scikit-helmert"):
#   
#     encoder = ce.HelmertEncoder(
#       cols = cat_labels, drop_invariant = True
#     )
# 
#   elif (encoding == "scikit-helmert"):
# 
#     encoder = ce.HelmertEncoder(
#       cols = cat_labels, drop_invariant = True
#   )
#   
#   elif (encoding == "scikit-james-stein"):
# 
#     encoder = ce.JamesSteinEncoder(
#       cols = cat_labels, drop_invariant = True
#   )
# 
#   elif (encoding == "scikit-polynomial"):
# 
#     encoder = ce.PolynomialEncoder(
#       cols = cat_labels, drop_invariant = True
#   )
# 
#   elif (encoding == "scikit-woe"):
# 
#     encoder = ce.WOEEncoder(
#       cols = cat_labels, drop_invariant = True
#   )
# 
#   elif (encoding == "scikit-binary"):
# 
#     encoder = ce.BinaryEncoder(
#       cols = cat_labels, drop_invariant = True
#   )  
#   
#   elif (encoding == "scikit-onehot"):
# 
#     encoder = ce.OneHotEncoder(
#       cols = cat_labels, drop_invariant = True
#   )   
#   return(encoder)



