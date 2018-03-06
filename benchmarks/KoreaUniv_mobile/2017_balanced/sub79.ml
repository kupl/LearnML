(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int


let rec bsum : branch -> int
= fun b ->
match b with
| SimpleBranch (a,b) -> b
| CompoundBranch (a,b) -> 
let (p,q) = b in bsum p + bsum q


let rec msum : mobile -> int
= fun m -> let (a,b)=m in (bsum a) + (bsum b) 


let rec balanced : mobile -> bool
= fun m ->
match m with
| (SimpleBranch (a,b), SimpleBranch (c,d)) -> if a*b = c*d then true else false
| (CompoundBranch(a,b), SimpleBranch (c,d)) -> 
if a*(msum b) = c*d then balanced b else false
| (SimpleBranch(a,b), CompoundBranch (c,d)) -> 
if a*b = c*(msum d) then balanced d else false
| CompoundBranch (a,b), CompoundBranch(c,d) -> 
if a*(msum b) = c*(msum d) then balanced b && balanced d else false