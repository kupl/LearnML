
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
	
	let rec totalWeight : mobile -> int
	= fun mob -> match mob with
	| (SimpleBranch (l1, w1) , SimpleBranch (l2, w2)) -> 		 w1 + w2
	| (SimpleBranch (l1, w1) , CompoundBranch (l2, mob2)) -> w1 + (totalWeight mob2)
	| (CompoundBranch (l1, mob1) , SimpleBranch (l2, w2)) -> w2 + (totalWeight mob1)
	| (CompoundBranch (l1, mob1) , CompoundBranch (l2, mob2)) -> (totalWeight mob1) + (totalWeight mob2)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
	| (SimpleBranch (l1, w1) , SimpleBranch (l2, w2)) ->		 (l1 * w1 = l2 * w2)
	| (SimpleBranch (l1, w1) , CompoundBranch (l2, mob2)) -> (l1 * w1 = l2 * (totalWeight mob2)) && (balanced mob2)
	| (CompoundBranch (l1, mob1) , SimpleBranch (l2, w2)) -> (l2 * w2 = l1 * (totalWeight mob1)) && (balanced mob1)
	| (CompoundBranch (l1, mob1), CompoundBranch (l2, mob2)) -> 
		(l1 * (totalWeight mob1) = l2 * (totalWeight mob2)) && (balanced mob1) && (balanced mob2)
