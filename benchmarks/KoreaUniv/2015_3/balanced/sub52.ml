  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec weight : branch -> int
  =fun (branch) -> match branch with
|SimpleBranch(a,b) -> b
|CompoundBranch(a,(b1,b2)) -> weight(b1) + weight(b2)

  let rec torque : branch -> int
  =fun (branch) -> match branch with
|SimpleBranch(a,b) -> a*b
|CompoundBranch(a,(b1,b2)) ->a*(weight(b1) + weight(b2))


  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match (lb,rb) with
|SimpleBranch(x,y),SimpleBranch(z,w) -> if torque (lb) = torque (rb) then true else false

|CompoundBranch(x,y),SimpleBranch(z,w) -> if torque (rb) = torque (lb)&&balanced(y) then true else false
|SimpleBranch(x,y), CompoundBranch(z,w) -> if torque (lb) = torque (rb) && balanced(w) then true else false
|CompoundBranch(x,y),CompoundBranch(z,w) -> if (torque(lb)=torque(rb) &&balanced(y) && balanced(w)) then true else false
