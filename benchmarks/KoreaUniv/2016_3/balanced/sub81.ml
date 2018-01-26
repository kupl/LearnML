  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec subweight mob = match mob with
		(SimpleBranch(a,b), SimpleBranch(c,d))->b+d
	|	(SimpleBranch(a,b), CompoundBranch(c,d))-> b+subweight(d)
	|	(CompoundBranch(a,b), SimpleBranch(c,d)) -> d+ subweight(b)
	|	(CompoundBranch(a,b), CompoundBranch(c,d)) -> subweight(b) + subweight(d)

  let rec balanced : mobile -> bool
  = fun mob ->match mob with
		(SimpleBranch(a,b), SimpleBranch(c,d))-> if (a*b) = (c*d) then true else false
	|	(SimpleBranch(a,b), CompoundBranch(c,d))->( if(a*b) = (c*subweight(d)) then true else false) &&balanced(d)
	|	(CompoundBranch(a,b), SimpleBranch(c,d)) ->( if (a*subweight(b)) = (c*d) then true else false) && balanced(b)
	|	(CompoundBranch(a,b), CompoundBranch(c,d)) ->( if (a*subweight(b)) = (c*subweight(d)) then true else false) &&balanced(b) &&balanced(d)
