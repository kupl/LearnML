
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob -> let rec fweight : mobile -> int = fun fw -> match fw with
			| (SimpleBranch (l1, w1), SimpleBranch (l2, w2))
			-> if l1 * w1 = l2 * w2 then w1 + w2
				 else -1000000
			| (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) 
			-> if l1 * w1 = l2 * (fweight m2) then w1 + (fweight m2)
				 else -1000000
			| (CompoundBranch (l1, m1), SimpleBranch (l2, w2))
			-> if l1 * (fweight m1) = l2 * w2 then (fweight m1) + w2
				 else -1000000
			| (CompoundBranch (l1, m1), CompoundBranch (l2, m2))
			-> if l1 * (fweight m1) = l2 * (fweight m2) then (fweight m1) + (fweight m2)
				 else -1000000 in
	match mob with
	| (SimpleBranch (l11, w11), SimpleBranch (l22, w22)) 
	-> l11 * w11 = l22 * w22
	| (SimpleBranch (l11, w11), CompoundBranch (l22, m22)) 
	-> l11 * w11 = l22 * (fweight m22)
	| (CompoundBranch (l11, m11), SimpleBranch (l22, w22)) 
	-> l11 * (fweight m11) = l22 * w22
	| (CompoundBranch (l11, m11), CompoundBranch (l22, m22)) 
	-> l11 * (fweight m11) = l22 * (fweight m22);;