
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec computeWeight : mobile -> int
  = fun mob -> match mob with
    | (SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> ll * lw + rl * rw
    | (SimpleBranch(ll,lw), CompoundBranch(rl,rm)) -> ll * lw + rl * computeWeight(rm)
    | (CompoundBranch(ll,lm), SimpleBranch(rl,rw)) -> ll * computeWeight(lm) + rl * rw
    | (CompoundBranch(ll,lm), CompoundBranch(rl,rm)) -> ll * computeWeight(lm) + rl * computeWeight(rm);;
  let rec balanced : mobile -> bool
  = fun mob -> match mob with
    | (SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> if(ll * lw = rl * rw) then true else false
    | (SimpleBranch(ll,lw), CompoundBranch(rl,rm)) -> if(ll * lw = rl * computeWeight(rm)) then balanced(rm) && true else false
    | (CompoundBranch(ll,lm), SimpleBranch(rl,rw)) -> if(ll * computeWeight(lm) = rl * rw) then balanced(lm) && true else false
    | (CompoundBranch(ll,lm), CompoundBranch(rl,rm)) -> if(ll * computeWeight(lm) = rl * computeWeight(rm)) then balanced(lm) && balanced(rm) else false;;
