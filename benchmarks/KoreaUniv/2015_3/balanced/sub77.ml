	type mobile = branch * branch
	and branch = SimpleBranch of length * weight
			   | CompoundBranch of length * mobile
	and length = int
	and weight = int


	let rec scale : branch -> int
	=fun branch ->
		match branch with 
		 | SimpleBranch (len, x) -> x
		 | CompoundBranch (len, (br1,br2)) -> scale br1 + scale br2

	let rec balanced : mobile -> bool 
	=fun (lb,rb) -> 
		match lb with
		 | SimpleBranch (len1, x1) -> 
			(match rb with
			 | SimpleBranch (len2, x2) -> if (len1 * (scale lb) = len2 * (scale rb)) then true else false
			 | CompoundBranch (len3, mobile3) -> if (balanced mobile3 = true) && (len1 * (scale lb) = len3 * (scale rb)) then true else false
			)
		 | CompoundBranch (len5, mobile5) -> 
			(match rb with
			 | SimpleBranch (len6, x6) -> if (balanced mobile5 = true) && (len5 * (scale lb) = len6 * (scale rb)) then true else false
			 | CompoundBranch (len7, mobile7) -> if (balanced mobile5 = true) && (balanced mobile7 = true) && 
													(len5 * (scale lb) = len7 * (scale rb)) then true else false
			)
