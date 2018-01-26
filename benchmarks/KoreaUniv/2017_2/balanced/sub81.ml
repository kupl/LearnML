(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> 
	let rec mass : branch -> int
	= fun cb ->
		match cb with 
		| SimpleBranch (ll,ww) -> ww
		| CompoundBranch (ll,mm) -> match mm with
																| (bb1, bb2) -> (mass bb1)+(mass bb2)
	in let rec torque : branch -> int 
	= fun b ->
		match b with 
		| SimpleBranch (l,w) -> l*w
		| CompoundBranch (l,m) -> match m with
															| (b1, b2) -> l*((mass b1)+(mass b2))
	in match m with
	| (b1,b2) -> match (b1,b2) with
							| (SimpleBranch (_,_),SimpleBranch (_,_)) -> if (torque b1)=(torque b2) then true else false
							| (CompoundBranch (_,m1),SimpleBranch (_,_)) -> if (balanced m1)&&((torque b1)=(torque b2)) then true else false
							| (SimpleBranch (_,_),CompoundBranch (_,m2)) -> if (balanced m2)&&((torque b1)=(torque b2)) then true else false
							| (CompoundBranch (_,m1), CompoundBranch (_,m2)) -> if ((balanced m1)&&(balanced m2))&&((torque b1)=(torque b2)) then true else false
 