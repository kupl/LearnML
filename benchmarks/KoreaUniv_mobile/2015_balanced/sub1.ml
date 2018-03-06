type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
=fun (lb, rb) -> match (lb, rb) with 
|(SimpleBranch (_, _), SimpleBranch (_, _))->if ((getForce (lb) = getForce (rb)) ) then true else false
|(SimpleBranch (_,_), CompoundBranch (_,a))->if ((getForce (lb) = getForce (rb)) && balanced(a)) then true else false
|(CompoundBranch (_,a), SimpleBranch (_,_))->if ((getForce (lb) = getForce (rb)) && balanced(a)) then true else false
|(CompoundBranch (_,a), CompoundBranch (_,b))->if ((getForce (lb) = getForce (rb))&& balanced(a) && balanced(b)) then true else false
and getWeight : branch -> int
=fun (mb) -> match mb with 
|SimpleBranch (a, b) -> b
|CompoundBranch (a, (b, c)) -> getWeight(b)+getWeight(c)
and getForce : branch -> int
=fun (mb) -> match mb with 
|SimpleBranch (a, b) -> a*b
|CompoundBranch (a, (b, c)) ->a*(getWeight(b)+getWeight(c))