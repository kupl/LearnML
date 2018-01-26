
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
  let rec temp : mobile -> int
  = fun mob -> match mob with
      | (SimpleBranch(l,w),SimpleBranch(ll,ww)) -> w+ww
	  | (SimpleBranch(l,w),CompoundBranch(ll,mm)) -> w + temp(mm)
	  | (CompoundBranch(l,m),SimpleBranch(ll,ww)) -> temp(m) + ww
	  | (CompoundBranch(l,m),CompoundBranch(ll,mm)) -> temp(m) + temp(mm)
  let rec balanced : mobile -> bool
  = fun mob -> match mob with
      | (SimpleBranch(l,w),SimpleBranch(ll,ww)) -> if (l*w)=(ww*ll) then true else false
	  | (SimpleBranch(l,w),CompoundBranch(ll,mm)) -> if (l*w)=(ll*temp(mm)) && balanced(mm)=true then true else false
	  | (CompoundBranch(l,m),SimpleBranch(ll,ww)) -> if (l*temp(m))=(ll*ww) && balanced(m)=true then true else false
	  | (CompoundBranch(l,m),CompoundBranch(ll,mm)) -> if (l*temp(m))=(ll*temp(mm)) && balanced(m)=true && balanced(mm)=true then true else false
