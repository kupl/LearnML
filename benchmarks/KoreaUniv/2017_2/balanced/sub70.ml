(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> 

	let rec weight : branch -> int
	= fun b ->
		match b with
		| CompoundBranch(k,l) -> 
			begin
				match l with
				| x,y -> (weight x) + (weight y)
			end
		| SimpleBranch(k,l) -> l
	in

	let rec torque : branch -> int
	= fun b ->
		match b with
		| CompoundBranch(a,b) ->
			begin
				match b with
				| x,y -> a * ((weight x) + (weight y))
			end
		| SimpleBranch(a,b) -> a*b
	in
		
	match m with
	| a,b -> 
		let left =
			begin
				match a with
				| CompoundBranch(k,l) -> (balanced l)
				| SimpleBranch(a,b) -> true
			end
		in
		let right =
			begin
				match b with
				| CompoundBranch(k,l) -> (balanced l)
				| SimpleBranch(a,b) -> true
			end
		in
		((torque a)==(torque b)) && left && right
