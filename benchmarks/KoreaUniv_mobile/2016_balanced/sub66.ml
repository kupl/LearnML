  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec weights : mobile -> int
	=fun mob ->
	match mob with
	| SimpleBranch(a,b), SimpleBranch(c,d) -> b+d
	| SimpleBranch(a,b), CompoundBranch(c,d) -> b+ weights(d)
	| CompoundBranch(a,b), SimpleBranch(c,d) -> weights(b)+d
	| CompoundBranch(a,b), CompoundBranch(c,d) -> weights(b)+weight(d)

  let rec balanced : mobile -> bool
  = fun mob -> (* TODO *)
	match mob with
	| SimpleBranch(a,b),SimpleBranch(c,d) -> if a*b = c*d then true else false
	| SimpleBranch(a,b),CompoundBranch(c,d) -> if (a*b = c*weights(d)) && balanced(d) then true else false
	| CompoundBranch(a,b),SimpleBranch(c,d) -> if  a*weights(b)=c*d && balanced(b) then true else false
	| CompoundBranch(a,b),CompoundBranch(c,d) -> if a*weights(b)=c*weights(d) && balanced(b) && balanced(d) then true else false
	| _->false
	

