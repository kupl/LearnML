
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob -> 
  (match mob with 
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> l1 * w1 == l2 * w2
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> if (balanced m2) then l1 * w1 == l2 * (total_weight m2) else false
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> if (balanced m1) then l1 * (total_weight m1) == l2 * w2 else false
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> if (balanced m1) && (balanced m2) then l1 * (total_weight m1) == l2 * (total_weight m2) else false)

  and total_weight : mobile -> int
  = fun mob -> 
  (match mob with
  | (SimpleBranch (l1, w1), SimpleBranch(l2, w2)) -> w1 + w2 
  | (SimpleBranch (l1, w1), CompoundBranch(l2, m2)) -> w1 + (total_weight m2)
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (total_weight m1) + w2
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (total_weight m1) + (total_weight m2))
