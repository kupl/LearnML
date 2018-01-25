  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
let rec totalWeight m : mobile -> int
  =fun m -> match m with 
  	| (SimpleBranch(_,w1), SimpleBranch(_,w2)) -> w1 + w2
  	| (CompoundBranch(_,m1), SimpleBranch(_,w2)) -> (totalWeight m1) + w2
  	| (SimpleBranch(_,w1), CompoundBranch(_,m2)) -> (totalWeight m2) + w1
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb, rb with
  	| (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) -> if (l1 * w1) = (l2 * w2) then true else false
  	| (CompoundBranch(l1,m1), SimpleBranch(l2,w2)) -> if (l1 * (totalWeight m1)) = (l2 * w2) then true else false 
  	| (SimpleBranch(l1,w1), CompoundBranch(l2,m2)) -> if (l1 * w1) = (l2 * (totalWeight m2)) then true else false;;
