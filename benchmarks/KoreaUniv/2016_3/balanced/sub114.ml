
  type  mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec weight2 mobile =
let rec weight1 branch = match branch with
SimpleBranch (l, w) -> w
|CompoundBranch (l, m) -> weight2 (m) in
match mobile with
(SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
|(SimpleBranch (l1, w1), CompoundBranch (l2, m)) -> w1 + weight1 (CompoundBranch (l2, m))
|(CompoundBranch (l1, m), SimpleBranch (l2, w2)) -> w2 + weight1 (CompoundBranch (l1, m))
|(CompoundBranch (l1, w1), CompoundBranch(l2, w2)) -> weight1 (CompoundBranch (l1, w1)) +weight1 (CompoundBranch (l2,w2))

let rec weight1 branch = match branch with
SimpleBranch (l, w) -> w
|CompoundBranch (l, m) -> weight2 (m)

let balanced : mobile -> bool

 = fun mob -> match mob with
|(SimpleBranch(l1,w1), SimpleBranch(l2,w2))  -> if w1*l1 = w2*l2 then true else false
|(SimpleBranch(l1,w1), CompoundBranch(l2,m)) -> if w1*l1 = weight2(m)*l2 then true else false
|(CompoundBranch(l1,m), SimpleBranch(l2,w2)) -> if l1*weight2(m) = l2*w2 then true else false
|(CompoundBranch(l1,w1), CompoundBranch(l2,w2)) -> if l1*weight2(w1) = l2*weight2(w2) then true else false;;
