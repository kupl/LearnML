
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec calc_w : mobile -> int
	= fun mob ->
	match mob with
	| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
	| (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> w1+calc_w(m2)
	| (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> calc_w(m1)+w2
	| (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> calc_w(m1)+calc_w(m2)

  let rec balanced : mobile -> bool
  = fun mob -> (* TODO *)
	match mob with
	| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if l1*w1=l2*w2 then true else false
	| (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> 
		if balanced(m2)=false then false 
		else if l1*w1=l2*calc_w(m2) then true else false
	| (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) ->
		if balanced(m1)=false then false
		else if l1*calc_w(m1)=l2*w2 then true else false
	| (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) ->
		if balanced(m1)=false || balanced(m2)=false then false
		else if l1*calc_w(m1)=l2*calc_w(m2) then true else false