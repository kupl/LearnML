  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec total_weight : branch -> int
	= fun b -> match b with
	SimpleBranch (l, w) -> w
	|CompoundBranch (l, m) ->
		(match m with
		(b1, b2) -> (total_weight b1) + (total_weight b2))

	let rec  balanced : mobile -> bool
	=fun (lb,rb) -> match (lb, rb) with
	(SimpleBranch (l1, w1), SimpleBranch(l2, w2)) ->
		if (l1 * w1) = (l2 * w2) then true else false
	|(CompoundBranch (l1, m1), SimpleBranch(l2, w2)) ->
		if l1 * (total_weight (CompoundBranch(l1, m1))) = (l2 * w2) then 
		balanced m1 else false
	|(SimpleBranch (l1, w1), CompoundBranch(l2, m2)) ->
		if (l1 * w1) = l2 * (total_weight(CompoundBranch(l2, m2))) then
		balanced m2 else false
	|(CompoundBranch (l1, m1), CompoundBranch(l2, m2)) ->
		if l1 * (total_weight (CompoundBranch(l1, m1))) = l2 * (total_weight
		(CompoundBranch(l2, m2))) then (balanced m1) && (balanced m2) else false
