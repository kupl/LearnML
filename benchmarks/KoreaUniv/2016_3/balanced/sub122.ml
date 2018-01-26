
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec weight a =
  match a with
  | (SimpleBranch(x, y), SimpleBranch(w, z)) -> y + z
  | (SimpleBranch(x, y), CompoundBranch(w, z)) -> y + (weight z)
  | (CompoundBranch(x, y), SimpleBranch(w, z)) -> (weight y) + z
  | (CompoundBranch(x, y), CompoundBranch(w, z)) -> (weight y) + weight z

  let rec balanced : mobile -> bool
  = fun mob -> 
  match mob with
  | (SimpleBranch(x, y), SimpleBranch(w, z)) -> if x*y = w*z then true else false 
  | (SimpleBranch(x, y), CompoundBranch(w, z)) -> if (x*y = w*(weight z)) && balanced z then true else false
  | (CompoundBranch(x, y), SimpleBranch(w, z)) -> if (x*(weight y) = w*z) && balanced y then true else false
  | (CompoundBranch(x, y), CompoundBranch(w, z)) -> if (x*(weight y) = w*(weight z)) && balanced y && balanced z then true else false 
