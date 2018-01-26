
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob -> (* raise NotImplemented *)
		let rec cal_weight : mobile -> int
		= fun mob ->
			match mob with
				SimpleBranch(l1,w1), SimpleBranch(l2,w2) -> w1 + w2
			|	CompoundBranch(l1,m1), SimpleBranch(l2,w2) -> cal_weight(m1) + w2
			| SimpleBranch(l1,w1), CompoundBranch(l2,m2) -> w1 + cal_weight(m2)
			| CompoundBranch(l1,m1), CompoundBranch(l2,m2) -> cal_weight(m1) + cal_weight(m2)
		in
		match mob with
			CompoundBranch(l1,m1), CompoundBranch(l2,m2) ->
				if cal_weight(m1) * l1 = cal_weight(m2) * l2 then true else false
		|	CompoundBranch(l1,m1), SimpleBranch(l2,w2) ->
				if cal_weight(m1) * l1 = l2 * w2 then true else false
		| SimpleBranch(l1,w1), CompoundBranch(l2,m2) ->
				if l1 * w1 = cal_weight(m2) * l2 then true else false
		| SimpleBranch(l1,w1), SimpleBranch(l2,w2) ->
				if l1 * w1 = l2 * w2 then true else false