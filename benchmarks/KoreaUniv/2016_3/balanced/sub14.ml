
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
    
  let rec balanced : mobile -> bool
  = fun mob -> 
    match mob with
    | SimpleBranch (a, b), SimpleBranch (x, y) -> if a*b=x*y then b+y else false
    | CompoundBranch (a, b), SimpleBranch (x, y) -> match b with
             | SimpleBranch (x, y), SimpleBranch (l, m) -> if x*y=l*m then balanced SimpleBranch(a, y+m), SimpleBranch(x, y) else false
             | CompoundBranch (x, y), SimpleBranch (l, m) -> balanced CompoundBranch (x, ((balanced CompoundBranch (x, y)), SimpleBranch (l, m)))
             | SimpleBranch (x, y), CompoundBranch (l, m, n) -> balanced CompoundBranch (balanced SimpleBranch (x, y), (balanced CompoundBranch (l, m, n)))
