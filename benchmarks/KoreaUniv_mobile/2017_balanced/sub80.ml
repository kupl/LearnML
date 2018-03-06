(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> (* TODO *)
let rec impl _m =
  match _m with
  | (left, right) ->
  let under b =
    match b with
    | SimpleBranch(l, w) -> w
    | CompoundBranch(l, sm) -> impl sm in
  let bv b w =
    match b with
    | SimpleBranch(l, _) -> l * w
    | CompoundBranch(l, _) -> l * w in
  let lw = under left in
  let rw = under right in
  if (bv left lw) = (bv right rw) then lw + rw
  else 0 in
(impl m) <> 0;;