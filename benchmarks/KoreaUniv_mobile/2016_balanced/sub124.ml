
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
	  let rec getWeight : mobile -> int 
	  = fun mob -> match mob with
		| (SimpleBranch(leftLength,leftWeight), SimpleBranch(rightLength,rightWeight)) -> leftWeight + rightWeight
		| (SimpleBranch(leftLength,leftWeight), CompoundBranch (rightLength, mob1)) -> leftWeight + getWeight (mob1)
		| (CompoundBranch(leftLength, mob2), SimpleBranch (rightLength,rightWeight)) -> getWeight(mob2) + rightWeight
		| (CompoundBranch(leftLength, mob3), CompoundBranch (rightLength, mob4)) -> getWeight (mob3) + getWeight (mob4)

	  let rec balanced : mobile -> bool
	  = fun mob -> match mob with
		| (SimpleBranch(leftLength,leftWeight), SimpleBranch(rightLength,rightWeight)) -> if leftLength*leftWeight = rightLength*rightWeight then true else false 
		| (SimpleBranch(leftLength,leftWeight), CompoundBranch (rightLength, mob1)) -> if balanced(mob1) && leftLength*leftWeight = rightLength*getWeight(mob1) then true else false
		| (CompoundBranch(leftLength, mob2), SimpleBranch (rightLength,rightWeight)) -> if balanced(mob2) && leftLength*getWeight(mob2) = rightLength*rightWeight then true else false
		| (CompoundBranch(leftLength, mob3), CompoundBranch (rightLength, mob4)) -> if balanced(mob3) && balanced(mob4) && leftLength*getWeight(mob3) = rightLength*getWeight(mob4) then true else false
