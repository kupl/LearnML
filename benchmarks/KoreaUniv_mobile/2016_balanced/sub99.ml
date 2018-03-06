
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec weight_branch branch =
	match branch with
	| SimpleBranch (l, w) -> w
	| CompoundBranch (l, m) -> weight_mobile (m)

	and weight_mobile mobile =
		match mobile with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
	| (SimpleBranch (l1, w1), CompoundBranch (l2, w2)) -> w1 + weight_branch (CompoundBranch (l2, w2))
	| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) -> weight_branch (CompoundBranch (l1, w1)) + w2
	| (CompoundBranch (l1, w1), CompoundBranch (l2, w2)) -> weight_branch (CompoundBranch (l1, w1)) + weight_branch (CompoundBranch (l2, w2))

	let check_balance mob =
	match mob with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if w1*l1 = w2*l2 then true else false
	| (SimpleBranch (l1, w1), CompoundBranch (l2, w2)) -> if w1*l1 = (weight_branch (CompoundBranch (l2, w2)))*l2 then true else false
	| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) -> if (weight_branch (CompoundBranch (l1, w1)))*l1 = w2*l2 then true else false
	| (CompoundBranch (l1, w1), CompoundBranch (l2, w2)) -> if (weight_branch (CompoundBranch (l1, w1)))*l1 = (weight_branch (CompoundBranch (l2, w2)))*l2 then true else false
	
	let rec balanced : mobile -> bool
  = fun mob ->
	match mob with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> check_balance (mob)
	| (SimpleBranch (l1, w1), CompoundBranch (l2, w2)) ->
		if (check_balance (mob) = true)&&(check_balance (w2) = true) then true else false
	| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) ->
		if (check_balance (mob) = true)&&(check_balance (w1) = true) then true else false
	| (CompoundBranch (l1, w1) , CompoundBranch (l2, w2)) ->
		if (check_balance (mob) = true)&&(check_balance (w1) = true)&&(check_balance (w2) = true) then true else false;;
