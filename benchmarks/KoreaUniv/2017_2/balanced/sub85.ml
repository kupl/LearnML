(*problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
			| CompoundBranch of length * mobile
and length = int
and weight = int

let rec sum_weight : mobile->int = fun b->
match b with
	|(SimpleBranch(a,b),SimpleBranch(c,d)) -> b+d
	|(SimpleBranch(a,b),CompoundBranch(c,d)) -> b+(sum_weight d)
	|(CompoundBranch(a,b),SimpleBranch(c,d)) -> d+(sum_weight b)
	|(CompoundBranch(a,b),CompoundBranch(c,d)) -> (sum_weight b)+(sum_weight d)
;;
let rec balanced : mobile->bool = fun m->
match m with
	|(SimpleBranch(a,b),SimpleBranch(c,d)) ->
		if (a*b)=(c*d) then true
		else false 
	|(CompoundBranch(a,b),SimpleBranch(c,d)) ->
		if ((balanced b) = true && (sum_weight b)*a=d*c) then true
		else false 
	|(SimpleBranch(a,b),CompoundBranch(c,d)) ->
		if ((balanced d) = true && (sum_weight d)*c=b*a) then true
		else false 
	|(CompoundBranch(a,b),CompoundBranch(c,d)) ->
		if ((balanced b) = true && (balanced d) = true && (sum_weight b)*a=(sum_weight d)*c)
			then true
		else false
;;