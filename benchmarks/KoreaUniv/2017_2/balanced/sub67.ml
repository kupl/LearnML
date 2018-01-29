(*problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
			|CompoundBranch of length * mobile
and length = int
and weight = int

let rec value mo =
	match mo with
	|SimpleBranch(l,w) -> l*w
	|CompoundBranch(l,subm) -> 
		(match subm with
		|(b1,b2)->l * (value b1+ value b2 ))


let balanced : mobile -> bool
= fun m -> match m with
|(b1,b2) -> if (value b1) = (value b2) then true else false
