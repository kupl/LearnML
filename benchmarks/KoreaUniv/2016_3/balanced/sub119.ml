
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec w : branch -> int
  = fun br -> match br with
        | SimpleBranch(a,b) -> b
        | CompoundBranch(a,b) ->  match b with
                |(c,d) -> w c + w d 

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
        | (SimpleBranch(a,b),SimpleBranch(c,d)) -> if a*w(SimpleBranch(a,b)) = c*w(SimpleBranch(c,d)) then true else false
        | (SimpleBranch(a,b),CompoundBranch(c,d)) -> if a*w(SimpleBranch(a,b)) = c*w(CompoundBranch(c,d)) then balanced(d) else false
        | (CompoundBranch(a,b),SimpleBranch(c,d)) -> if a*w(CompoundBranch(a,b)) = c*w(SimpleBranch(c,d)) then balanced(b) else false
        | (CompoundBranch(a,b),CompoundBranch(c,d)) -> if a*w(CompoundBranch(a,b)) = c*w(CompoundBranch(c,d)) then balanced(d) && balanced(b) else false
