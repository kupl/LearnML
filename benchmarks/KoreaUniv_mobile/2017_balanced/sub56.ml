(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> 
let rec wb b = match b with 
	| SimpleBranch(q,w) -> w
	| CompoundBranch(q,w) ->
		match w with 
		| (z,x) -> ((wb z)+(wb x))
		in let rec bals m = match m with
			| SimpleBranch(q,w) -> q*(wb m)
			| CompoundBranch(q,w) -> q*(wb m)
			in let rec bal m = match m with
				| SimpleBranch(a,b),SimpleBranch(c,d) -> if (bals (SimpleBranch(a,b)) = bals (SimpleBranch(c,d))) then true else false
				| CompoundBranch(a,b),SimpleBranch(c,d) -> if (bal b) then (if (bals (CompoundBranch(a,b)) = bals (SimpleBranch(c,d))) then true else false) else false
				| SimpleBranch(a,b),CompoundBranch(c,d) -> if (bal d) then (if (bals (SimpleBranch(a,b)) = bals (CompoundBranch(c,d))) then true else false) else false
				| CompoundBranch(a,b),CompoundBranch(c,d) -> if (bal b && bal d) then (if (bals (CompoundBranch(a,b)) = bals (CompoundBranch(c,d))) then true else false) else false
				in bal m
