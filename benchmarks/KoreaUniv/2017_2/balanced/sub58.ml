(* problem 6 *)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int
let rec sum_w
= fun m -> match m with
  | (SimpleBranch (x1, y1), SimpleBranch (x2, y2)) -> y1 + y2
  | (SimpleBranch (x1, y1), CompoundBranch (x2, y2)) -> y1 + (sum_w y2)
  | (CompoundBranch (x1, y1), SimpleBranch (x2, y2)) -> (sum_w y1) + y2
  | (CompoundBranch (x1, y1), CompoundBranch (x2, y2)) -> (sum_w y1) + (sum_w y2)
let rec sum 
= fun b -> match b with
  | SimpleBranch (x1, y1) -> x1
  | CompoundBranch (x1, y1) -> sum_w y1
let rec total 
= fun b -> match b with
  | SimpleBranch (x1, y1) -> x1*y1
  | CompoundBranch (x1, y1) -> x1 * (sum b)
let rec balanced : mobile -> bool
= fun m -> match m with
  | (SimpleBranch (x1, y1), SimpleBranch (x2, y2)) -> if (x1*y1) = (x2*y2) then true else false
  | (SimpleBranch (x1, y1), CompoundBranch (x2, y2)) -> if (total (SimpleBranch (x1, y1)) = (total (CompoundBranch (x2, y2)))) then true&&(balanced y2) else false
  | (CompoundBranch (x1, y1), SimpleBranch (x2, y2)) -> if (total (CompoundBranch (x1, y1)) = (total (SimpleBranch (x2, y2)))) then true&&(balanced y1) else false
  | (CompoundBranch (x1, y1), CompoundBranch (x2, y2)) -> if (total (CompoundBranch (x1, y1)) = (total (CompoundBranch (x2, y2)))) then true&&(balanced y1)&&(balanced y2) else false
