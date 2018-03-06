  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  
  let cmp l1 w1 l2 w2 =
    if (l1*w1) = (l2*w2) then w1+w2
    else -1
  let rec bal mobile =
    match mobile with
    | (SimpleBranch (l1,w1), SimpleBranch (l2,w2)) -> cmp l1 w1 l2 w2
    | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> cmp l1 w1 l2 (bal m2)
    | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> cmp l1 (bal m1) l2 w2
    | (CompoundBranch(l1, m1), CompoundBranch (l2, m2)) -> cmp l1 (bal m1) l2 (bal m2)

  let balanced : mobile -> bool
  =fun (lb,rb) -> if bal (lb, rb) < 0 then false else true
