
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec weight : mobile -> int
	= fun n ->
		match n with
		|(SimpleBranch(_, a), SimpleBranch(_, b)) -> a + b
		|(SimpleBranch(_, a), CompoundBranch(_, b)) -> a + weight(b)
		|(CompoundBranch(_, a), SimpleBranch(_, b)) -> weight(a) + b
		|(CompoundBranch(_, a), CompoundBranch(_, b)) -> weight(a) + weight(b)

  let balanced : mobile -> bool
  = fun mob ->
		match mob with
		|(SimpleBranch(a, b), SimpleBranch(c, d)) ->
			if a*b = c*d then true else false
		|(SimpleBranch(a, b), CompoundBranch(c, d)) ->
			if a*b = c*weight(d) then true else false
		|(CompoundBranch(a, b), SimpleBranch(c, d)) ->
			if a*weight(b) = c*d then true else false
		|(CompoundBranch(a, b), CompoundBranch(c, d)) ->
			if a*weight(b) = c*weight(d) then true else false