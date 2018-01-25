(* 2012210066 조현상 hw3.ml *)
(* Problem 1*)
	type mobile = branch * branch
	and branch = SimpleBranch of lenght * weight
						 | CompoundBranch of lenght * mobile 
	and lenght = int
	and weight = int

	let balanced : mobile -> bool 
	= fun (lb,rb) -> false
	let rec ball : mobile -> int 
	= fun (lb,rb) -> 0
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

(*Problem 2*)
type exp = V of var
				| P of var * exp
				| C of exp * exp
     and var = string

     let check : exp -> bool
		 =fun e -> true 
		 let rec checktest a b = match a with 
																	 |V x -> List.mem x b
																	 |P (x, y) -> checktest y (x::b)
																	 |C (x, y) -> checktest x b && checktest y b;;
		 let check exp = checktest exp [];;
