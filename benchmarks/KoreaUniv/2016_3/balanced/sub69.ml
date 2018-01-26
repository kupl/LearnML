
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
	
	let torque : (int * int) -> int
	= fun (a,b) -> a*b
	
	let rec comweight : branch -> int
	= fun brc -> match brc with
		| SimpleBranch (l1,w1) -> w1
		| CompoundBranch (l1,(brc1,brc2)) -> comweight brc1 + comweight brc2
	
  let rec balanced : mobile -> bool
  = fun mob -> 
		let (brc1,brc2) = mob in
		match mob with
			| (SimpleBranch (l1,w1), SimpleBranch (l2,w2)) -> if torque (l1,w1) = torque (l2,w2) then true else false
			| (SimpleBranch (l1,w1), CompoundBranch (l2,m2)) -> if balanced m2 && torque (l1,w1) = torque (l2,comweight brc2) then true else false
			| (CompoundBranch (l1,m1), SimpleBranch (l2,w2)) -> if balanced m1 && torque (l2,w2) = torque (l1,comweight brc1) then true else false
			| (CompoundBranch (l1,m1), CompoundBranch (l2,m2)) -> if balanced m1 && balanced m2 && torque (l1,comweight brc1) = torque (l2,comweight brc2) then true else false
		 