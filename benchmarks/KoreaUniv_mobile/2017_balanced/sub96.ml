(* problem 6*)
type mobile = branch * branch   (* left and rigth branches *)
and branch = SimpleBranch of length * weight
          | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> let rec eval exp  = match exp with
                            | (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
                            | (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> w1+(eval m2)
                            | (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> (eval m1) + w2
                            | (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> (eval m1) + (eval m2)
          in let rec test exp = match exp with
                          | (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if l1*w1 != l2*w2 then false
                          else true
                          | (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> if l1*w1 != l2*(eval m2) then false
                          else test m2
                          | (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> if l1*(eval m1) != l2*w2 then false
                          else test m1
                          | (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> if l1*(eval m1) != l2*(eval m2) then false
                          else test m1 && test m2
                            in test m