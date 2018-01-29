
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec raiseweight : mobile -> int
  = fun mob -> match mob with
              |(SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> lw + rw
              |(CompoundBranch(ll,lw), SimpleBranch(rl,rw)) -> raiseweight(lw) + rw
              |(SimpleBranch(ll,lw), CompoundBranch(rl,rw)) -> lw + raiseweight(rw)
              |(CompoundBranch(ll,lw), CompoundBranch(rl,rw)) -> raiseweight(lw) + raiseweight(rw)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
							|(SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> if (ll*lw)=(rl*rw) then true else false
							|(CompoundBranch(ll,lw), SimpleBranch(rl,rw)) -> if (((ll*(raiseweight(lw)))=(rl*rw))&&(balanced(lw))) then true else false 
							|(SimpleBranch(ll,lw), CompoundBranch(rl,rw)) -> if (((ll*lw)=(rl*raiseweight(rw)))&&balanced(rw)) then true else false 
							|(CompoundBranch(ll,lw), CompoundBranch(rl,rw)) -> if (((ll*raiseweight(lw))=(rl*raiseweight(rw)))&&balanced(lw)&&balanced(rw)) then true else false
