
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob -> (* TODO *)
	match mob with
	| SimpleBranch(a,b),SimpleBranch(c,d) when a*b = c*d -> true
	| SimpleBranch(a,b),CompoundBranch(c,d) when a*b = c*weight(d) && balanced(d) -> true
	| CompoundBranch(a,b),SimpleBranch(c,d) when a*weight(b)=c*d && balanced(b) -> true
	| CompoundBranch(a,b),CompoundBranch(c,d) when a*weight(b)=c*weight(d) && balanced(b) && balanced(d) -> true   
	| _->false
	
and weight : mobile -> int
=fun mob ->
match mob with
| SimpleBranch(a,b), SimpleBranch(c,d) -> b+d
| SimpleBranch(a,b), CompoundBranch(c,d) -> b+ weight(d)
| CompoundBranch(a,b), SimpleBranch(c,d) -> weight(b)+d
| CompoundBranch(a,b), CompoundBranch(c,d) -> weight(b)+weight(d)