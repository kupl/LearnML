
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let abs : int -> int
	= fun x -> if x>0 then x else 0-x

	let rec balance_weight : mobile -> int
	= fun mob -> match mob with
	| (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> w1+w2
	| (SimpleBranch(l1, w1), CompoundBranch(l2, m2)) -> w1+balance_weight(m2)
	| (CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> balance_weight(m1)+w2
	| (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> balance_weight(m1)+balance_weight(m2)

  and balanced : mobile -> bool
	= fun mob -> match mob with
	| (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> if l1*w1 = l2*w2 then true else false
	| (SimpleBranch(l1, w1), CompoundBranch(l2, m2)) -> if l1*w1 = l2*balance_weight(m2) then balanced(m2) else false
	| (CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> if l1*balance_weight(m1) = l2*w2 then balanced(m1) else false
	| (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> if l1*balance_weight(m1) = l2*balance_weight(m2) then balanced(m1) && balanced(m2) else false
