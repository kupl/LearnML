
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec sum : mobile -> int
  = fun mob ->
  match mob with
  l,r -> match l,r with
	  SimpleBranch(a,b),SimpleBranch(c,d) ->b+d 
	 |SimpleBranch(a,b),CompoundBranch(c,d) -> b + (sum d)
	 |CompoundBranch(a,b),SimpleBranch(c,d) -> (sum b) + d
	 |CompoundBranch(a,b),CompoundBranch(c,d) -> (sum b) + (sum d)

  
  let rec balanced : mobile -> bool
  = fun mob ->
  match mob with 
  l,r -> match l,r with
	 SimpleBranch(a,b),SimpleBranch(c,d) -> if(a*b=c*d) then true else false
	|SimpleBranch(a,b),CompoundBranch(c,d) ->
						 if((balanced d)&&(sum d)*c=a*b)
						  then true else false
	|CompoundBranch(a,b),SimpleBranch(c,d) ->
						 if((balanced b)&&(sum b)*a=c*d)
						  then true else false
	|CompoundBranch(a,b),CompoundBranch(c,d) ->
		 if((balanced b)&&(balanced d)&&(sum b)*a=(sum d)*c) then true 
		 else false