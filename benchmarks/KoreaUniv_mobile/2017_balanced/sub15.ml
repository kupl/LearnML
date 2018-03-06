(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec calWeight : branch -> int
= fun b -> match b with
	| SimpleBranch (len,w) -> w
	| CompoundBranch (len, mobile) -> match mobile with
					| (a,b) -> calWeight a + calWeight b
let rec calTorque : branch -> int
= fun b -> match b with
	| SimpleBranch (len,w) -> len * w 
	| CompoundBranch (len,mobile) -> match mobile with
					| (a,b) -> if (calTorque a = calTorque b) then len * (calWeight a + calWeight b) else -1
(* not same torque -> not balanced*)
let balanced : mobile -> bool
= fun m -> match m with
	| (left,right) -> if (calTorque left) = (calTorque right) then true else false 
