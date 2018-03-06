type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec getWeight : mobile -> weight
=fun (lb,rb) ->
  match lb, rb with
  |SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> w1 + w2
  |SimpleBranch (l1,w1), CompoundBranch (l2,(lb2,rb2)) -> w1 + getWeight (lb2,rb2)
  |CompoundBranch (l1,(lb1,rb1)), SimpleBranch (l2,w2) -> getWeight(lb1,rb1) + w2
  |CompoundBranch (l1,(lb1,rb1)), CompoundBranch (l2,(lb2,rb2)) -> getWeight (lb1,rb1) + getWeight (lb2,rb2)


let balanced : mobile -> bool
=fun (lb,rb) ->
  match lb, rb with
  |SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> l1*w1 = l2*w2
  |SimpleBranch (l1,w1), CompoundBranch(l2,m2) -> l1*w1 = l2*(getWeight m2)
  |CompoundBranch (l1,m1), SimpleBranch(l2,w2) -> l1*(getWeight m1) = l2*w2
  |CompoundBranch (l1,m1), CompoundBranch(l2,m2) -> l1*(getWeight m1) = l2*(getWeight m2)
 