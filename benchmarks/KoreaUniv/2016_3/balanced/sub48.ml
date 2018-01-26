
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec checkb : mobile -> int = fun i ->
	match i with
	| (SimpleBranch (l1,w1), SimpleBranch(l2,w2)) -> (w1+w2)
	| (SimpleBranch (l1,w1), CompoundBranch(l2,(c,d)))->(w1 + checkb (c,d))
	| (CompoundBranch (l1,(c,d)), SimpleBranch(l2,w2))->(w2 + checkb (c,d))
	| (CompoundBranch (l1, (c1,d1)),CompoundBranch(l2,(c2,d2))) ->
		(checkb(c1,d1) + checkb(c2,d2))
		
  let rec balanced : mobile -> bool
  = fun mob -> match mob with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> ((l1*w1) = (l2*w2))
	| (SimpleBranch (l1, w1), CompoundBranch (l2, (a, b))) -> 
	((balanced (a,b)) && ((l1*w1) = (l2*(checkb(a,b)))))
	| (CompoundBranch (l1, (a, b)), SimpleBranch (l2, w2)) ->
	((balanced (a,b)) && ((l2*w2) = (l1*(checkb(a, b)))))
	| (CompoundBranch (l1, (a1, b1)), CompoundBranch (l2, (a2, b2))) -> 
	((balanced (a1, b1)) && (balanced (a2, b2)) &&
	((l1*(checkb(a1,b1))) = (l2*(checkb (a2,b2)))))