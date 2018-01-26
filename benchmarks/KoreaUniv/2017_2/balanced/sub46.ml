(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec get_weight m =
  match m with
  |(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> (w1 + w2)
  |(SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> (w1 + get_weight m1)
  |(CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> (get_weight m1 + w2)
  |(CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> (get_weight m1) + (get_weight m2)

let rec balanced : mobile -> bool
= fun m -> match m with
  |(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> (l1 * w1) = (l2 * w2)
  |(SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> ((l1 * w1) = (l2 * get_weight m1)) && balanced m1
  |(CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> ((l1 * get_weight m1) = (l2 * w2)) && balanced m1
  |(CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> ((l1 * get_weight m1) = (l2 * get_weight m2)) && (balanced m1) && (balanced m2)
