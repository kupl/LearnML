(*problem 6*)

type mobile = branch * branch (*left and right branches*)
and branch = SimpleBranch of length * weight
| CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool = fun m ->
let rec bal : mobile -> int = fun m ->
match m with
|(SimpleBranch (l1,w1), SimpleBranch(l2,w2)) -> if(l1 * w1 = l2 * w2)
then w1+w2 else -1
|(SimpleBranch (l1,w1), CompoundBranch(l2,m2)) -> if(l1 * w1 = l2 * bal(m2))
then w1+bal(m2) else -1
|(CompoundBranch(l1,m1), SimpleBranch(l2,w2)) -> if(l1 * bal(m1) = l2 * w2)
then bal(m1)+w2 else -1
|(CompoundBranch(l1,m1), CompoundBranch(l2,m2)) -> if(l1 * bal(m1) = l2 * bal(m2)) then bal(m1)+bal(m2) else -1
in (bal(m)>0);;
