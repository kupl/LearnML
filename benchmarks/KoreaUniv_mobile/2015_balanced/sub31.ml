  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
                         
  let rec balanced : mobile -> bool
  =fun (lb,rb) ->
    let rec calc_weight : mobile -> int 
    =fun (l,r) ->
      match (l,r) with
      | (SimpleBranch (ll,lw), SimpleBranch (rl,rw)) -> lw + rw
      | (CompoundBranch (ll,lm), SimpleBranch (rl,rw)) -> 
        (calc_weight lm) + rw
      | (SimpleBranch (ll,lw), CompoundBranch (rl,rm)) ->
        lw + (calc_weight rm) 
      | (CompoundBranch (ll,lm), CompoundBranch (rl,rm)) ->
        (calc_weight lm) + (calc_weight rm)
    in match (lb,rb) with
    | (SimpleBranch (ll,lw), SimpleBranch (rl,rw)) ->
      if (ll*lw = rl*rw) then true
      else false
    | (CompoundBranch (ll,lm), SimpleBranch (rl,rw)) ->
      if ((balanced lm) && (ll*(calc_weight lm) = rl*rw)) then true
      else false
    | (SimpleBranch (ll,lw), CompoundBranch (rl,rm)) ->
      if ((balanced rm) && (ll*lw = rl*(calc_weight rm))) then true
      else false
    | (CompoundBranch (ll,lm), CompoundBranch (rl,rm)) ->
      if (((balanced lm) && (balanced rm)) && (ll*(calc_weight lm) = rl*(calc_weight rm))) then true
      else false
