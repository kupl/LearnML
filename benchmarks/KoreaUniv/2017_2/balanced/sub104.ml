(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> 
 let rec sum e = 
  match e with
  | (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) ->
    w1+w2
  | (SimpleBranch(l1,w1), CompoundBranch(l2,m2)) ->
    w1 + (sum m2)
  | (CompoundBranch(l1, m1), SimpleBranch(l2,w2)) ->
    (sum m1) + w2
  | (CompoundBranch(l1,m1), CompoundBranch(l2,m2)) ->
    (sum m1) + (sum m2)
  in let rec balance e = 
  match e with
  | (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) ->
    if l1 * w1 != l2 * w2 then false
    else true
  | (SimpleBranch(l1,w1), CompoundBranch(l2,m2)) ->
    if l1 * w1 != l2 * (sum m2) then false
    else balance m2
  | (CompoundBranch(l1, m1), SimpleBranch(l2,w2)) ->
    if l1 * (sum m1) != l2 * w2 then false
    else balance m1
  | (CompoundBranch(l1,m1), CompoundBranch(l2,m2)) ->
    if l1 * (sum m1) != l2 * (sum m2) then false
    else balance m1 && balance m2
  in balance m