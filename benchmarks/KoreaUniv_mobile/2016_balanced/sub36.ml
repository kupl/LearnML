
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob ->
		let rec get_weight : branch -> int
		= fun brnch ->
		begin
		match brnch with
		| SimpleBranch (l, w) -> w
		| CompoundBranch (l, (b1, b2)) -> get_weight b1 + get_weight b2
		end in
			begin
			let (b1, b2) = mob in
			match mob with
			| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if l1*w1 = l2*w2 then true else false
			| (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> if (balanced m2) && l1*w1 = l2*(get_weight b2) then true else false
			| (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> if (balanced m1) && l1*(get_weight b1) = l2*w2 then true else false
			| (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> if (balanced m1) && (balanced m2) && l1*(get_weight b1) = l2*(get_weight b2) then true else false
			end