(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec compound_to_simple : (length * mobile) -> (length * weight)
= fun (l, m) ->
	match m with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> (l, w1 + w2)
	| (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> (
		let (_, w2) = compound_to_simple (l2, m2) in
		(l, w1 + w2)
	)
	| (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (
		let (_, w1) = compound_to_simple (l1, m1) in
		(l, w1 + w2)
	)
	| (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (
		let (_, w1) = compound_to_simple (l1, m1) in
		let (_, w2) = compound_to_simple (l2, m2) in
		(l, w1 + w2)
	)
;;

let rec balanced : mobile -> bool
= fun m ->
	match m with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> (
		if (l1 * w1) == (l2 * w2) then true
		else false
	)
	| (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> (
		if (balanced m2) then (
			let (_, w2) = compound_to_simple (l2, m2) in
			if (l1 * w1) == (l2 * w2) then true
			else false
		)
		else false
	)
	| (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (
		if (balanced m1) then (
			let (_, w1) = compound_to_simple (l1, m1) in
			if (l1 * w1) == (l2 * w2) then true
			else false
		)
		else false
	)
	| (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (
		if (balanced m1) && (balanced m2) then (
			let (_, w1) = compound_to_simple (l1, m1) in
			let (_, w2) = compound_to_simple (l2, m2) in
			if (l1 * w1) == (l2 * w2) then true
			else false
		)
		else false
	)
;;