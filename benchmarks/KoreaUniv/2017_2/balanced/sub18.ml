(*problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
						|CompoundBranch of length * mobile
and length = int
and weight = int

let rec amount : mobile -> int
= fun m ->
match m with
(SimpleBranch (l1, w1), SimpleBranch (l2,w2)) -> w1 + w2
|(SimpleBranch (l1, w1), CompoundBranch (l2, (SimpleBranch (l3, w3), SimpleBranch (l4, w4)))) -> w1 + w3 + w4
|(SimpleBranch (l1, w1), CompoundBranch (l2, mobile)) -> w1 + (amount mobile)
|(CompoundBranch (l1, (SimpleBranch(l2, w2), SimpleBranch(l3, w3))), SimpleBranch(l4,w4)) -> w2 + w3 + w4
|(CompoundBranch (l1, mobile), SimpleBranch(l2, w2)) -> (amount mobile) + w2 
|(CompoundBranch (l1, (SimpleBranch(l2, w2), SimpleBranch(l3, w3))), CompoundBranch(l4, (SimpleBranch(l5, w5), SimpleBranch(l6, w6)))) -> w2 + w3 + w5 + w6
|(CompoundBranch (l1, mobile1), CompoundBranch (l2, mobile2)) -> (amount mobile1) + (amount mobile2)

let rec balanced : mobile -> bool
= fun m ->
match m with
(SimpleBranch (l1, w1) , SimpleBranch (l2, w2)) -> (l1 * w1) == (l2 * w2)
|(SimpleBranch (l1, w1) , CompoundBranch (l2, (SimpleBranch (l3, w3), SimpleBranch (l4, w4)))) -> ((l3 * w3) == (l4 * w4)) && ((l1*w1) == (l2 *(w3 + w4))) 
|(SimpleBranch (l1, w1), CompoundBranch (l2, mobile)) -> (balanced mobile) && ((l1 * w1) == (l2 * (amount mobile)))
|(CompoundBranch (l1, (SimpleBranch(l2, w2), SimpleBranch(l3, w3))), SimpleBranch(l4,w4)) -> ((l2*w2)==(l3*w3)) && ((l1 * (w2+w3))==(l4*w4))
|(CompoundBranch (l1, mobile), SimpleBranch(l2, w2)) -> (balanced mobile) && ((l1 * (amount mobile)) == (l2 * w2))
|(CompoundBranch (l1, (SimpleBranch(l2, w2), SimpleBranch(l3, w3))), CompoundBranch(l4, (SimpleBranch(l5, w5), SimpleBranch(l6, w6)))) -> ((l2 * w2) == (l3 * w3)) && ((l5 * w5) == (l6 * w6)) && ((l1 * (w2 + w3)) == (l4 * (w5 + w6)))
|(CompoundBranch (l1, mobile1), CompoundBranch (l2, mobile2)) -> (balanced mobile1) && (balanced mobile2) && ((l1 * (amount mobile1)) == (l2 * (amount mobile2)))
