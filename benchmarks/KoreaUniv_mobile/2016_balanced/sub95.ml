
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec calcuWeight : branch * branch -> int
  = fun m ->
       ( match m with
        |(SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> if((ll * lw) = (rl * rw)) then (lw + rw) else 0
        |(SimpleBranch(ll,lw), CompoundBranch(l,r)) -> if(ll * lw) = (l *(calcuWeight r)) then lw + (l * (calcuWeight r)) else 0
	|(CompoundBranch(l,r),SimpleBranch(rl,rw)) -> if (rl * rw) = (l *(calcuWeight r)) then rw + (l *(calcuWeight r)) else 0
	|(CompoundBranch(ll,lr),CompoundBranch(rl,rr)) -> if(ll *(calcuWeight lr)=(rl *(calcuWeight rr))) then (calcuWeight lr) + (calcuWeight rr) else 0
	|_-> raise(Failure "NotProper")
)

  let balanced : mobile -> bool
  = fun mob -> 
	match mob with 
	|(CompoundBranch(ll,lr),CompoundBranch(rl,rr)) -> if(ll *(calcuWeight lr)=(rl *(calcuWeight rr))) then true else false
	|(SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> if((ll * lw) = (rl * rw)) then true else false
	|(SimpleBranch(ll,lw), CompoundBranch(l,r)) -> if(ll * lw) = (l *(calcuWeight r)) then true else false
	|(CompoundBranch(l,r),SimpleBranch(rl,rw)) -> if (rl * rw) = (l *(calcuWeight r)) then true else false
	|_-> raise(Failure "NotProper")