
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec get_sum : mobile -> int
	= fun mob ->
		match mob with
		| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
 	  | (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> w1 + get_sum(m2)
    | (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> get_sum(m1) + w2
    | (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> get_sum(m1) + get_sum(m2)

  let rec balanced : mobile -> bool
  = fun mob ->
		match mob with
		| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if l1*w1 = l2*w2 then true else false
		| (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) ->
			if balanced m2 then
			begin
				if get_sum(m2) * l2 = l1 * w1 then true else false
			end
			else false
		| (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) ->
			if balanced m1 then
			begin	
				if get_sum(m1) * l1 = l2 * w2 then true else false
			end
			else false
		| (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) ->
			if balanced m1 && balanced m2 then
			begin
				if get_sum(m1) * l1 = get_sum(m2) * l2 then true else false
			end
			else false
