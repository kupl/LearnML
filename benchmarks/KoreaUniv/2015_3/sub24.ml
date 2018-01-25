(*Problem 1*)
type mobile = branch * branch

and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
           and length = int
	   and weight = int

let rec bal m = match m with
	    	| (SimpleBranch(a,b),SimpleBranch(c,d)) -> c+d
let rec balanced m = match m with
		| (SimpleBranch(a,b),SimpleBranch(c,d)) -> if a*b=c*d then true else false
		| (CompoundBranch(a,b),SimpleBranch(c,d)) -> balanced (SimpleBranch (a,bal b),SimpleBranch(c,d))		
		| (CompoundBranch(a,b),CompoundBranch(c,d)) -> balanced (SimpleBranch(a,bal b),SimpleBranch(c,bal d)) 
		| (SimpleBranch(a,b),CompoundBranch(c,d)) -> balanced (SimpleBranch(a,b),SimpleBranch(c,bal d))


(*Problem 2*)
type exp = V of var
	  | P of var * exp
          | C of exp * exp
              and var = string
  
let check : exp -> bool
  =fun e -> let rec check x y =
		match x with    
		|V x1 -> List.mem x1 y
		|C (x1, y1) -> check x1 y && check y1 y 
		|P (x1, y1) -> check y1 (x1::y) in check e[]



