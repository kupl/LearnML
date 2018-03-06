(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weigh : branch -> int
= fun b ->
	(match b with
	| SimpleBranch (l,w) -> w
	| CompoundBranch (l,m) -> 
		let (left, right) = m in
			(weigh left)+(weigh right)
	)

let rec balanced : mobile -> bool
= fun m -> 
	let (left, right) = m in
		(match left with
		| SimpleBranch(l1,w1) -> 
			(match right with
			| SimpleBranch(l2,w2) -> if l1*w1 = l2*w2 then true else false
			| CompoundBranch(l2,m2) -> if balanced m2 && l1*w1 = l2*(weigh right) then true else false) 
		| CompoundBranch(l1,m1) -> if balanced m1 then
			(match right with
			| SimpleBranch(l2,w2) -> if l1*(weigh left) = l2*w2 then true else false
			| CompoundBranch(l2,m2) -> if balanced m2 && l1*(weigh left) = l2*(weigh right) then true else false)
			else false
		)