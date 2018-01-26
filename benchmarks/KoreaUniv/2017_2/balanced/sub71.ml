(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int
;;

let rec compare : mobile -> int
= fun m ->
match m with
(a,b) -> match a,b with
| SimpleBranch(l,w), SimpleBranch(l2,w2) -> w+w2
| SimpleBranch(l,w), CompoundBranch(l2,m1) -> w+(compare m1)
| CompoundBranch(l,m1), SimpleBranch(l2,w2) -> w2+(compare m1)
| CompoundBranch(l,m1), CompoundBranch(l2,m2) -> (compare m1) + (compare m2);;


let rec balanced : mobile -> bool
= fun m ->
match m with
(a,b) -> match a,b with
| SimpleBranch (l,w), SimpleBranch (l2,w2) -> if (l*w)=(l2*w2) then true else false
| SimpleBranch (l,w), CompoundBranch (l2,m1) -> if (balanced m1)&&((l*w)=(l2*(compare m1))) then true else false
| CompoundBranch (l,m1), SimpleBranch (l2,w2) -> if (balanced m1)&&((l2*w2)=(l*(compare m1))) then true else false
| CompoundBranch (l,m1), CompoundBranch (l2,m2) -> if (balanced m1)&&(balanced m2)&&((compare m1)=(compare m2)) then true else false;;
