type mobile = branch * branch     
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec plus : branch -> int
= fun p ->
match p with
| SimpleBranch(m, n) -> n
| CompoundBranch(m, (n, o)) -> (plus n) + (plus o);;

let rec mu1 : branch -> int
= fun mo ->
match mo with
| SimpleBranch(a, b) -> a * (plus mo)
| CompoundBranch(a, (b, c)) -> a * (plus mo);;

let rec balanced : mobile -> bool
= fun mob ->
let rec ba1 : branch -> bool
= fun br ->
match br with
| SimpleBranch(1, w) -> true
| CompoundBranch(1, (w1, w2)) -> if (mu1 w1) = (mu1 w2) then (ba1 w1) && (ba1 w2)
else false
in
match mob with
| (a, b) -> ((ba1 a) && (ba1 b)) && ((mu1 a) = (mu1 b))