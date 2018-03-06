
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec sum : mobile -> int
	= fun mob ->
		match mob with
		| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
		| (SimpleBranch (l1, w1), CompoundBranch (l2, mobile)) -> w1 + (sum mobile) 
		| (CompoundBranch (l1, mobile), SimpleBranch (l2, w2)) -> (sum mobile) + w2
		| (CompoundBranch (l1, mobile1), CompoundBranch (l2, mobile2)) -> (sum mobile1) + (sum mobile2) 

  let rec balanced : mobile -> bool
  = fun mob -> 
		match mob with
		| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> 
			if (l1 * w1) = (l2 * w2) then true else false
		| (SimpleBranch (l1, w1), CompoundBranch (l2,  mobile2)) ->
			if (l1 * w1) = (l2 * (sum mobile2)) then true else false
		| (CompoundBranch (l1, mobile1), SimpleBranch (l2, w2)) ->
			if (l1 * (sum mobile1)) = (l2 * w2) then true else false
		| (CompoundBranch (l1, mobile1), CompoundBranch (l2, mobile2)) ->
			if (l1 * (sum mobile1)) = (l2 * (sum mobile2)) then true else false