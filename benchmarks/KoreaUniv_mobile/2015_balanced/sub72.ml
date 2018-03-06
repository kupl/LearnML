(* 2012210066 조현상 hw3.ml *)
(* Problem 1*)
	type mobile = branch * branch
	and branch = SimpleBranch of lenght * weight
						 | CompoundBranch of lenght * mobile 
	and lenght = int
	and weight = int

	let rec ball mobile =
		match mobile with 
		| (SimpleBranch(a,b),SimpleBranch(c,d)) -> if a*b=c*d then b+d else -100
		| (CompoundBranch(a,b),SimpleBranch(c,d)) ->ball (SimpleBranch(a,ball b),SimpleBranch(c,d))
		| (SimpleBranch(a,b),CompoundBranch(c,d)) ->ball (SimpleBranch(a,b),SimpleBranch(c,ball d))
	  | (CompoundBranch(a,b),CompoundBranch(c,d)) -> ball (SimpleBranch(a,ball b),SimpleBranch(c,ball d))
	let rec balanced mobile =
	  match mobile with 
		| (SimpleBranch(a,b),SimpleBranch(c,d)) -> if a*b=c*d then true else false 
    | (CompoundBranch(a,b),SimpleBranch(c,d)) ->balanced (SimpleBranch(a,ball b),SimpleBranch(c,d))
    | (SimpleBranch(a,b),CompoundBranch(c,d)) ->balanced (SimpleBranch(a,b),SimpleBranch(c,ball d))
    | (CompoundBranch(a,b),CompoundBranch(c,d)) -> balanced (SimpleBranch(a,ball b),SimpleBranch(c,ball d));;
