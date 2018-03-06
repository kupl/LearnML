type mobile = branch * branch
and branch = SimpleBranch of length * weight
						|CompoundBranch of length * mobile
and length = int
and weight = int

let rec ival : mobile -> int
=fun x->
			match x with
			|(SimpleBranch (a,b),SimpleBranch(c,d))
		  	-> if a*b=c*d then b+d else 0
			|(CompoundBranch (a,b), SimpleBranch (c,d))
				->ival (SimpleBranch (a, ival(b)), SimpleBranch (c,d))
			|(SimpleBranch (a,b), CompoundBranch (c,d))
				->ival (SimpleBranch(a,b), SimpleBranch (c,ival(d)))
			|(CompoundBranch(a,b), CompoundBranch(c,d))
				->ival (SimpleBranch (a,ival(b)), SimpleBranch (c,ival(d)))

let balanced : mobile->bool
=fun (lb, rb) -> if (ival(lb,rb)>0) then true else false
;;
