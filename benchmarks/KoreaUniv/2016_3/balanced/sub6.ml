
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec mobileWeight : mobile -> weight
	=fun mobile ->
		match mobile with
			| (SimpleBranch(length1, weight1), SimpleBranch(length2, weight2))
				-> weight1 + weight2
			| (CompoundBranch(length1, mobile1), SimpleBranch(length2, weight2))
				-> mobileWeight mobile1 + weight2
			| (SimpleBranch(length1, weight1), CompoundBranch(length2, mobile2))
				-> weight1 + mobileWeight mobile2
			| (CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2))
				-> mobileWeight mobile1 + mobileWeight mobile2

  let rec balanced : mobile -> bool
  = fun mob -> 
		match mob with
			| (CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2))
				-> if balanced(mobile1) && balanced(mobile2) then
						if length1 * mobileWeight(mobile1) = length2 * mobileWeight(mobile2) then true
						else false
					else false
			| (SimpleBranch(length1, weight1), CompoundBranch(length2, mobile2))
				-> if balanced(mobile2) then
						if length1 * weight1 = length2 * mobileWeight(mobile2) then true
						else false
					else false
			| (CompoundBranch(length1, mobile1), SimpleBranch(length2, weight2))
				-> if balanced(mobile1) then
						if length1 * mobileWeight(mobile1) = length2 * weight2 then true
						else false
					else false
			| (SimpleBranch(length1, weight1), SimpleBranch(length2, weight2))
				-> if length1 * weight1 = length2 * weight2 then true
					else false