
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec wei (m)  =
		match m with
		| SimpleBranch (_, w) -> w
		| CompoundBranch (_, (mo1, mo2)) -> wei mo1 + wei mo2

	let pow (m) =
		match m with
		| SimpleBranch (l, w) -> l * w
		| CompoundBranch (l, (mo1, mo2)) -> l * (wei mo1 + wei mo2)

	let rec balanced : mobile -> bool
	= fun mob ->
		match mob with
		| (m1, m2) -> (
									(if pow (m1) = pow (m2) then true else false) &&
										(
										match m1, m2 with
										| (SimpleBranch(_,_),SimpleBranch(_,_)) -> true
										| (CompoundBranch(_,k1), CompoundBranch(_,k2)) -> balanced k1 && balanced k2
										| (SimpleBranch(_,_),CompoundBranch(_,k2)) -> balanced k2
										| (CompoundBranch(_,k1),SimpleBranch(_,_)) -> balanced k1
										)
									)