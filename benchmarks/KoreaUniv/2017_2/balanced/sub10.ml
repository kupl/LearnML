(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

(* Helper functions problem 6 *)
let rec branchWeight tWeight branch =
  match branch with
    |SimpleBranch(l, w) -> (tWeight + w)
    |CompoundBranch(l, nMob) -> begin
      match nMob with
        |(b1,b2) -> (branchWeight tWeight b1) + (branchWeight tWeight b2)
    end

let mobWeight mob =
match mob with
  |(left, right) -> (branchWeight 0 left) + (branchWeight 0 right)

let branchTorque tTorque branch =
  match branch with
    |SimpleBranch(l, w) -> (tTorque + (l*w))
    |CompoundBranch(l, nMob) -> (tTorque + (l*(mobWeight nMob)))
(* End of helper functions problem 6 *)

let rec balanced : mobile -> bool
= fun m ->
match m with
  |(left, right) -> if (branchTorque 0 left) = (branchTorque 0 right) then true
    &&
    begin
      match left with
        |SimpleBranch(l, w) -> true
        |CompoundBranch(l, nMob) -> balanced nMob
    end
    &&
    begin
      match right with
        |SimpleBranch(l, w) -> true
        |CompoundBranch(l, nMob) -> balanced nMob
    end else false