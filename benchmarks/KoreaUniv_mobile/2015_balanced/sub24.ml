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

