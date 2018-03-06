  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec get_weight : branch -> int
  =fun b -> match b with
  |SimpleBranch (l,w) -> w
  |CompoundBranch (l,(lb,rb)) -> (get_weight (lb)) + (get_weight (rb))
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb,rb with
  | SimpleBranch(l1,w1), SimpleBranch(l2,w2) -> ((l1 * w1) = (l2 * w2))
  | CompoundBranch(l,m), SimpleBranch(l2,w2) -> balanced(m) && ((l * get_weight(lb)) = (l2 * w2))
  | SimpleBranch(l1,w1), CompoundBranch(l,m) -> balanced(m) && ((l1 * w1) = (l * get_weight(rb)))
  | CompoundBranch(l1,m1), CompoundBranch(l2,m2) -> balanced(m1) && balanced(m2) && ((l1 * get_weight(lb)) = (l2 * get_weight(rb)))
  
  