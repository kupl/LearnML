type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec is_valid : mobile -> bool
= fun (lb, rb) ->
  let rec branch_is_valid : branch -> bool
  = fun b ->
    match b with
    | SimpleBranch (l, w) -> l > 0 && w > 0
    | CompoundBranch (l, m) -> l > 0 && is_valid m
  in
  (branch_is_valid lb) && (branch_is_valid rb)

let rec mobile_weight : mobile -> int
= fun m -> 
  match m with
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> w1 + (mobile_weight m2)
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (mobile_weight m1) + w2
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (mobile_weight m1) + (mobile_weight m2)

let rec balanced : mobile -> bool
= fun m -> 
  if is_valid m then
    match m with
    | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> (l1 * w1) = (l2 * w2)
    | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> ((l1 * w1) = (l2 * (mobile_weight m2))) && (balanced m2)
    | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (balanced m1) && ((l1 * (mobile_weight m1)) = (l2 * w2)) 
    | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (balanced m1) && (balanced m2) && ((l1 * (mobile_weight m1)) = (l2 * (mobile_weight m2)))
  else raise (Failure "Invalid")