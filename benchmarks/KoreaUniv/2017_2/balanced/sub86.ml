(*problem 6*)
 
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int
 
let rec balanced : mobile -> bool
= fun m -> 
let rec sum_weight br =
match br with
| SimpleBranch (l, w) -> w
| CompoundBranch (l, (sub1, sub2)) -> sum_weight sub1 + sum_weight sub2
in
let rec mul_weight br =
match br with
| SimpleBranch (l, w) -> l * w
| CompoundBranch (l, (sub1, sub2)) -> l * (sum_weight sub1 + sum_weight sub2)
in
match m with
| (topleft, topright) -> match topleft with
		| SimpleBranch (left_leng, left_weight) -> 
			(match topright with
			| SimpleBranch (right_leng, right_weight) -> if mul_weight topleft = mul_weight topright then true
							else false
			| CompoundBranch (right_leng, (left_sub, right_sub)) -> if (balanced (left_sub, right_sub) = true) then 
									if mul_weight topleft = mul_weight topright then true else false
								else false)
		| CompoundBranch (left_leng, (left_sub1, right_sub1)) -> if (balanced (left_sub1, right_sub1) = false) then false 
							else
							(match topright with
							| SimpleBranch (right_leng,right_weight) -> if mul_weight topleft = mul_weight topright then true
											else false
							| CompoundBranch (right_leng, (left_sub2, right_sub2)) -> if (balanced (left_sub2, right_sub2) = false) then false
											else if mul_weight topleft = mul_weight topright then true
											else false)