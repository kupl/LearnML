  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec cal_weight : mobile -> int
  = fun (lb,rb) -> match lb,rb with
  | SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> w1 + w2
  | SimpleBranch (l1,w1), CompoundBranch (l2,b2) -> w1 + cal_weight(b2)
  | CompoundBranch (l1,b1), SimpleBranch (l2,w2) -> w2 + cal_weight(b1)
  | CompoundBranch (l1,b1), CompoundBranch (l2,b2) -> cal_weight(b1) + cal_weight(b2)

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb,rb with
  | SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> if l1*w1 = l2*w2 then true else false
  | SimpleBranch (l1,w1), CompoundBranch (l2,b2) -> if balanced(b2) && (l1*w1 = l2*cal_weight(b2)) then true else false
  | CompoundBranch (l1,b1), SimpleBranch (l2,w2) -> if balanced(b1) && (l2*w2 = l1*cal_weight(b1)) then true else false
  | CompoundBranch (l1,b1), CompoundBranch (l2,b2) -> if balanced(b1) && balanced(b2) && (l1*cal_weight(b1) = l2*cal_weight(b2)) then true else false


