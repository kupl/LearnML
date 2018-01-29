
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob ->
		match mob with
		| (br, bl) -> if (torque br) = (torque bl) then true else false
	and torque : branch -> int
	= fun b ->
		match b with
		| SimpleBranch (l, w) -> l*w
		| CompoundBranch (l, m) ->
			if balanced m then
				(weight m)*l else -999
	and weight : mobile -> int
	= fun m ->
		match m with
		| (br, bl) -> 
			(match (br, bl) with
			 | (SimpleBranch (l, w), SimpleBranch (l2,w2)) -> w + w2
			 | (SimpleBranch (l, w), CompoundBranch (l2, m)) -> w + (weight m)
			 | (CompoundBranch (l, m), SimpleBranch (l2, w)) -> (weight m) + w
			 | (CompoundBranch (l, m), CompoundBranch (l2, m2)) -> (weight m) + (weight m2)) 
