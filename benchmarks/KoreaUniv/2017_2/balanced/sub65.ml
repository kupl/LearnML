(* problem 6*)
type mobile = branch * branch     (* left and right branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m ->
  let rec branchweight b =
    match b with
    | SimpleBranch (len, w) -> w
    | CompoundBranch (len, m) ->
      (match m with (left, right) ->
        (branchweight left) + (branchweight right)) in
  let torque b =
    match b with
    | SimpleBranch (len, w) -> len * w 
    | CompoundBranch (len, m) -> len * (branchweight b) in
  let branchbalanced b =
    match b with
    | SimpleBranch (len, w) -> true
    | CompoundBranch (len, m) -> balanced m in
  match m with (l, r) ->
    (branchbalanced l) && (branchbalanced r)
    && ((torque l) = (torque r))
