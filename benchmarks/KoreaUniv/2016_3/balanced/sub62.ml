
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec getWeight : mobile -> int
	= fun mob ->
		match mob with 
		|(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> w1+w2
		|(CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> getWeight(m1)+w2
		|(SimpleBranch(l1, w1), CompoundBranch(l2, m2))-> w1+getWeight(m2)
		|(CompoundBranch(l1, m1), CompoundBranch(l2, m2))-> getWeight(m1) + getWeight(m2)

  let rec balanced : mobile -> bool
  = fun mob -> 
	match mob with
		|(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> if (l1*w1)= (l2*w2) then true else false
		|(CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> if (l1*getWeight(m1)) = (l2*w2) && balanced m1 then true else false
		|(SimpleBranch(l1, w1), CompoundBranch(l2, m2))-> if (l2*getWeight(m2)) = (l1*w1) && balanced m2 then true else false
		|(CompoundBranch(l1, m1), CompoundBranch(l2, m2))-> if (l1*getWeight(m1))= (l2*getWeight(m2)) && balanced m1 && balanced m2 then true else false
