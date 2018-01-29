
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec total_weight : mobile -> int
  = fun mweight -> match mweight with
      (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
    | (CompoundBranch (a, b), SimpleBranch (l, w)) -> total_weight b + w
    | (SimpleBranch (l, w), CompoundBranch (a, b)) -> w + total_weight  b
    | (CompoundBranch (a1, b1), CompoundBranch (a2, b2)) -> total_weight b1 + total_weight b2

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
      (SimpleBranch(l1, w1), SimpleBranch(l2,w2)) -> if (l1 * w1) == (l2 * w2) then true else false
    | (CompoundBranch (a, b), SimpleBranch(l2, w2)) -> if (a*total_weight b) == (l2*w2) then true else false
    | (SimpleBranch(l1, w1), CompoundBranch(a,b)) -> if (a*total_weight b) == (l1*w1) then true else false
    | (CompoundBranch(a1, b1), CompoundBranch(a2, b2)) -> if (a1*total_weight b1) == (a2*total_weight b2) then true else false
