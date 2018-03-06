type mobile = branch * branch 
and branch = 
	SimpleBranch of length * weight 
	| CompoundBranch of length * mobile 
and length = int 
and weight = int 

let balanced : mobile -> bool =fun (lb,rb) -> false let rec wei p =
	match p with
	(SimpleBranch(a1,b1),CompoundBranch(a2,b2)) -> b1 + (wei b2)
	| (SimpleBranch(a1,b1), SimpleBranch(a2,b2)) -> b1 + b2
	| (CompoundBranch(a1,b1), SimpleBranch(a2,b2)) -> (wei b1) + b2
	| (CompoundBranch(a1,b1), CompoundBranch(a2,b2)) -> (wei b1) + (wei b2)

let rec balanced p = 
	match p with	
	(SimpleBranch(a1,b1),CompoundBranch(a2,b2)) -> if (a1 * b1) = (a2 * (wei b2)) then (balanced b2) else false
	| (SimpleBranch(a1,b1), SimpleBranch(a2,b2)) -> if (a1 * b1) = (a2 * b2) then true else false
	| (CompoundBranch(a1,b1), SimpleBranch(a2,b2)) -> if (a1 * (wei b1)) = (a2  * b2) then (balanced b1) else false
	| (CompoundBranch(a1,b1), CompoundBranch(a2,b2)) -> if (a1 * (wei b1)) = (a2 * (wei b2)) then (balanced b1) && (balanced b2) else false
