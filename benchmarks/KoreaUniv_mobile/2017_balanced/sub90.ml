(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec get_weight br
= match br with
	| SimpleBranch (l, w) -> w
	| CompoundBranch (l, m) -> (match m with
					| (left, right) -> ((get_weight left) + (get_weight right)))
let get_torque br
= match br with
	| SimpleBranch (l, w) -> l*(get_weight br)
	| CompoundBranch (l, m) -> l*(get_weight br)

let rec balanced : mobile -> bool
= fun m -> match m with
		| (left, right) -> (match left with
					| CompoundBranch (l, m) -> (match right with
									| CompoundBranch (l2,m2) -> if ((get_torque left) = (get_torque right)) && ((balanced m) = true) && ((balanced m2) = true) then true else false
									| SimpleBranch (l2, w2) -> if ((get_torque left) = (get_torque right)) && ((balanced m) = true) then true else false)
					| SimpleBranch (l, w) -> (match right with
									| CompoundBranch (l2, m2) -> if ((get_torque left) = (get_torque right)) && ((balanced m2) = true) then true else false
									| SimpleBranch (l2, w2) -> if (get_torque left) = (get_torque right) then true else false ))

