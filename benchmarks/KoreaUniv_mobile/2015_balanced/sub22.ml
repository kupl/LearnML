  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
 
  let rec tweight : mobile -> int
  =fun (lb,rb) -> match (lb,rb) with
  | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> w1+w2
  | (SimpleBranch(l1, w1), CompoundBranch(l2, m2)) | (CompoundBranch(l2, m2), SimpleBranch(l1, w1))
    -> w1+(tweight m2)
  | (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> (tweight m1) + (tweight m2)

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match (lb,rb) with
  | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> if l1*w1 = l2*w2 then true else false
  | (SimpleBranch(l1, w1), CompoundBranch(l2, m2)) | (CompoundBranch(l2, m2), SimpleBranch(l1, w1))
    -> if (balanced m2) && (tweight m2)*l2 = l1*w1 then true else false
  | (CompoundBranch(l1, m1), CompoundBranch(l2, m2))
    -> if (balanced m1) && (balanced m2) && (tweight m1)*l1 = (tweight m2)*l2 then true else false
