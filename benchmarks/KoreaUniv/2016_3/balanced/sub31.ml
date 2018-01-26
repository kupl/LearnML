
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec eval_w m = 
  match m with
  |(SimpleBranch(a,b),SimpleBranch(c,d)) -> b + d
  |(SimpleBranch(a,b),CompoundBranch(c,d)) -> b + (eval_w d)
  |(CompoundBranch(a,b),SimpleBranch(c,d)) -> (eval_w b) + d
  |(CompoundBranch(a,b),CompoundBranch(c,d)) -> (eval_w b) + (eval_w d)

  let rec balanced : mobile -> bool
  = fun mob -> 
  match mob with
  |(SimpleBranch(a,b),SimpleBranch(c,d)) -> if a * b = c * d then true else false
  |(SimpleBranch(a,b),CompoundBranch(c,d)) -> if (a * b = c * (eval_w d)) && balanced d then true else false
  |(CompoundBranch(a,b),SimpleBranch(c,d)) -> if balanced b && (a * (eval_w b) = c * d) then true else false
  |(CompoundBranch(a,b),CompoundBranch(c,d)) -> if a * (eval_w b) = c * (eval_w d) && balanced b && balanced d then true else false
