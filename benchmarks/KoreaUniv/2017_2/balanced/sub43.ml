(* problem 6*)
type mobile = branch * branch     (* left and right branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> (* TODO *)
	let rec force b =
		match b with
		| SimpleBranch (_,w) -> w
		| CompoundBranch(_,m) ->
			match m with (b1,b2) ->
				force b1 + force b2
	in
	let netf m =
		match m with (b1,b2) -> force b1 + force b2
	in
	let torque b = 	(*Clockwise is negative:Abondoned*)
		match b with
		| SimpleBranch (l,w) -> l*w
		| CompoundBranch (l,m) -> l*netf m
	in
	let chktqe m =
		match m with (b1,b2) ->
			if torque b1 = torque b2 then true else false
	in
	let rec subbal m =
		let a = 
			match m with (b1,_) ->
			match b1 with
			| SimpleBranch (_,_) -> true
			| CompoundBranch (_,mb) -> subbal mb
		in
		let b =
			match m with (_,b2) ->
			match b2 with
			| SimpleBranch (_,_) -> true
			| CompoundBranch (_,mb) -> subbal mb
		in
		let c =
			chktqe m
		in
			a&&b&&c
	in
		subbal m