	type mobile = branch * branch
	and branch = SimpleBranch of length * weight
    	       | CompoundBranch of length * mobile
	and length = int
	and weight = int

	let rec eval1 : mobile -> int
	= fun e -> 
		match e with
		| (SimpleBranch(a,b), SimpleBranch(c,d)) ->
			if a*b=c*d then b+d else -1
		| (CompoundBranch(a,b), SimpleBranch(c,d)) ->
			eval1(SimpleBranch(a, eval1(b)), SimpleBranch(c,d))
		| (SimpleBranch(a,b), CompoundBranch(c,d)) ->
			eval1(SimpleBranch(a,b), SimpleBranch(c, eval1(d)))
		| (CompoundBranch(a,b), CompoundBranch(c,d)) ->
			eval1(SimpleBranch(a, eval1(b)), SimpleBranch(c, eval1(d)))

	let balanced : mobile -> bool
	=fun (lb,rb) -> if eval1(lb,rb)=(-1) then false else true
