(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->
  let rec wt = fun b ->
    match b with
    | SimpleBranch (_, w) -> w
    | CompoundBranch (_, (left, right)) -> (wt left) + (wt right)
  in
  let ln = fun b ->
    match b with
    | SimpleBranch (l, _) -> l
    | CompoundBranch (l, _) -> l
  in
  let (left, right) = m in
  ((wt left) * (ln left)) = ((wt right) * (ln right))
