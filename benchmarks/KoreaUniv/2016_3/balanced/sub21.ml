
type mobile = branch * branch (*left and rigth branches*)
and branch = SimpleBranch of length * weight
					 | CompoundBranch of length * mobile
and length = int
and weight = int


let rec sumwe
	= fun a -> match a with
	| SimpleBranch (leng,weig) -> weig
	| CompoundBranch (leng,(left,right)) -> (sumwe left) + (sumwe right)


let mul
	= fun a -> match a with
	| SimpleBranch (leng,weig) -> leng*weig
	| CompoundBranch (leng, (left,right)) -> leng*((sumwe left) + (sumwe right))

let rec balanced : mobile -> bool
	= fun (left,right) -> match left with
	| SimpleBranch (leng1,weig1) -> (match right with
																	| SimpleBranch(leng2,weig2) -> if ((mul left) = (mul right)) then true else false
																	| CompoundBranch(leng2,(left1,right1)) -> if (( balanced(left1,right1) = true) && ((mul left) = (mul right))) then true else false)
	| CompoundBranch (leng1,(left1,right1)) -> if (balanced(left1,right1) = false)  then false else ( match right with
																																															| SimpleBranch(leng2,weig2) -> if ((mul left) = (mul right)) then true else false
																																															| CompoundBranch(leng2,(left2,right2)) -> if (balanced(left2,right2) = false) then false else if ((mul left) = (mul right)) then true else false)
