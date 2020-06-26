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
    
    encoder = ce.TargetEncoder(cols = cat_labels, drop_invariant = True)
  
  elif (encoding == "scikit-ordinal"):
  
    encoder = ce.OrdinalEncoder(cols = cat_labels, drop_invariant = True)
  
  elif (encoding == "scikit-backward-difference"):
  
    encoder = ce.BackwardDifferenceEncoder(
      cols = cat_labels, drop_invariant = True
    )
  
  elif (encoding == "scikit-helmert"):
  
    encoder = ce.HelmertEncoder(
      cols = cat_labels, drop_invariant = True
    )
  
  return(encoder)




