
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob ->

	 	let rec weight_of_mobile (mb) = match mb with
			|SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> w1 + w2
			|SimpleBranch (l1, w), CompoundBranch (l2, m) -> w + weight_of_mobile (m)
			|CompoundBranch (l1, m), SimpleBranch (l2, w) -> weight_of_mobile (m) + w
			|CompoundBranch (l1, m1), CompoundBranch (l2, m2) -> weight_of_mobile (m1) + weight_of_mobile (m2)
		in

		match mob with
			|SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> if l1 * w1 = l2 * w2 then true else false
			|SimpleBranch (l1, w), CompoundBranch (l2, m) -> if l1 * w = l2 * weight_of_mobile (m)
				then true && balanced (m) else false
			|CompoundBranch (l1, m), SimpleBranch (l2, w) -> if l1 * weight_of_mobile (m) = l2 * w
				then true && balanced (m) else false
			|CompoundBranch (l1, m1), CompoundBranch (l2, m2) -> if l1 * weight_of_mobile (m1) = l2 * weight_of_mobile (m2)
				then true && balanced (m1) && balanced (m2) else false