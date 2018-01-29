
(* problem 6*)
type mobile = branch * branch 
and branch = SimpleBranch of length * weight | CompoundBranch of length * mobile
and length = int
and weight = int
let rec calWeight : branch -> int = fun b ->
   match b with
   |SimpleBranch(len, wei) -> wei
   |CompoundBranch(len, mob) -> calWeight (fst mob) + calWeight (snd mob);;

let rec calLength : branch -> int = fun b ->
   match b with
   |SimpleBranch(len, wei) -> len
   |CompoundBranch(len, mob) -> len

let balanced : mobile -> bool = fun m ->
   if calLength (fst m) * calWeight (fst m) = calLength (snd m) * calWeight (snd m) then true
   else false;;

