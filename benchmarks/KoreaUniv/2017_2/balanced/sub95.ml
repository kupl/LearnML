(*problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
						|CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight : mobile -> int
= fun m -> match m with
	(SimpleBranch (l1,w1),SimpleBranch (l2,w2)) -> w1+w2
	|(SimpleBranch (l1,w1),CompoundBranch (l2,m1)) -> w1+weight(m1)
	|(CompoundBranch (l1,m1),SimpleBranch (l2,w2)) -> weight(m1)+w2
	|(CompoundBranch (l1,m1),CompoundBranch(l2,m2)) -> weight(m1)+weight(m2);;

let rec balanced : mobile -> bool
= fun m -> match m with
	(SimpleBranch (l1,w1),SimpleBranch (l2,w2)) -> if (l1*w1=l2*w2) then true else false
	|(SimpleBranch (l1,w1),CompoundBranch (l2,m1)) -> if (l1*w1=l2*(weight(m1))) then true else false
	|(CompoundBranch (l1,m1),SimpleBranch (l2,w2)) -> if (l1*(weight(m1))=l2*w2) then true else false
	|(CompoundBranch (l1,m1),CompoundBranch (l2,m2)) -> if (l1*(weight(m1))=l2*(weight(m2))) then true else false;;
