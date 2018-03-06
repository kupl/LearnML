
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec mobileVal : mobile -> int
  = fun mob ->
  	match mob with
  	| (SimpleBranch(ll, lw), SimpleBranch(rl, rw)) -> lw+rw
   	| (CompoundBranch(ll, lm), SimpleBranch(rl, rw)) -> (mobileVal lm)+rw
  	| (SimpleBranch(ll, lw), CompoundBranch(rl, rm)) -> lw+(mobileVal rm)
  	| (CompoundBranch(ll, lm), CompoundBranch(rl, rm)) -> (mobileVal lm) + (mobileVal rm)

  let rec balanced : mobile -> bool
  = fun mob -> 
  	match mob with
  	| (SimpleBranch(ll, lw), SimpleBranch(rl, rw)) -> (ll*lw) = (rl*rw)
  	| (CompoundBranch(ll, lm), SimpleBranch(rl, rw)) -> (balanced lm) && (((mobileVal lm)*ll) = (rl*rw))
  	| (SimpleBranch(ll, lw), CompoundBranch(rl, rm)) -> ((ll*lw) = (rl*(mobileVal rm))) && (balanced rm)
  	| (CompoundBranch(ll, lm), CompoundBranch(rl, rm)) -> (balanced lm) && (balanced rm) && ((ll*(mobileVal lm)) = (rl*(mobileVal rm)))
