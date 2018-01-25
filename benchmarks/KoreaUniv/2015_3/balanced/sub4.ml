	type mobile = branch * branch
	and branch = SimpleBranch of length * weight
           	| CompoundBranch of length * mobile
	and length = int
	and weight = int

	let rec balanced : mobile -> bool
	= fun (lb,rb) -> false

	let rec cal_weight : branch -> weight
	= fun b ->
		match b with
		SimpleBranch (l,w) -> w
	| CompoundBranch (l,m) -> 
			(match m with
				(lb,rb) -> (cal_weight lb) + (cal_weight rb));;

	let rec cal_torque : branch -> int
	= fun b ->
		match b with
		SimpleBranch (l,w) -> l * w
	|	CompoundBranch (l,m) -> 
			(match m with
				(lb,rb) -> l * (cal_weight lb + cal_weight rb));;

	let rec balanced : mobile -> bool
	= fun (lb,rb) ->
		match lb,rb with
		SimpleBranch (l1,w1),SimpleBranch (l2,w2) -> 
			if cal_torque lb = cal_torque rb then true else false
	| SimpleBranch (l1,w),CompoundBranch (l2,m) ->
			if (balanced m = true) && (cal_torque lb = cal_torque rb)
				then true else false
	| CompoundBranch (l1,m),SimpleBranch (l2,w) -> 
			if (balanced m = true) && (cal_torque lb = cal_torque rb)
				then true else false
	| CompoundBranch (l1,m1),CompoundBranch (l2,m2) ->
			if (balanced m1 = true) && (balanced m2 = true) 
			&& (cal_torque lb = cal_torque rb)
				then true else false;;
