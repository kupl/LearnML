(* problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int;;

let get_length : branch -> int
= fun b -> match b with
            |SimpleBranch (l, w) -> l
            |CompoundBranch (l, m) -> l;;

let rec branch_weight : branch -> int
= fun b -> 
  let mobile_weight : mobile -> int
  = fun m -> match m with
              |(left, right) -> (branch_weight left) + (branch_weight right)
  in
  match b with
    |SimpleBranch (l, w) -> (get_length b) * w
    |CompoundBranch (l, m) -> (get_length b) * (mobile_weight m);;

let balanced : mobile -> bool
= fun m -> match m with
            |(left, right) -> (branch_weight left) == (branch_weight right);;