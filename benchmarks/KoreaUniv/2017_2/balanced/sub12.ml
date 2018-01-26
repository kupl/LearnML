(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec sum_weight : mobile -> int
= fun m -> match m with
           | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> w1+w2
           | (CompoundBranch(l1, m1), SimpleBranch(l2, w1)) -> w1+sum_weight(m1)
           | (SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> w1+sum_weight(m1)
           | (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> sum_weight(m1)+sum_weight(m2)

let find_part_balance : mobile ->int
= fun m -> match m with
           | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> if l1*w1=l2*w2 then 1 else 0
           | (SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> if l1*w1=l2*(sum_weight m1) then 1 else 0
           | (CompoundBranch(l1, m1), SimpleBranch(l2, w1)) -> if l2*w1=l1*(sum_weight m1) then 1 else 0
           | (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> if l1*(sum_weight m1)=l2*(sum_weight m2) then 1 else 0
           
let rec find_total_balance : mobile ->int
=fun m -> match m with
          | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> find_part_balance m
          | (SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> (find_part_balance m)*(find_total_balance m1)
          | (CompoundBranch(l1, m1), SimpleBranch(l2, w1)) -> (find_part_balance m)*(find_total_balance m1)
          | (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> (find_part_balance m)*(find_total_balance m1)*(find_total_balance m2)

let balanced : mobile -> bool
= fun m -> if (find_total_balance m)=1 then true else false
