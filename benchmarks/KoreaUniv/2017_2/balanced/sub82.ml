  (* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec force br = 
    match br with 
        SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> 
            w1+w2
        | SimpleBranch (l1,w1), CompoundBranch (l2,w2) -> 
            w1+l2*(force w2)
        | CompoundBranch (l1,w1), SimpleBranch (l2,w2) -> 
            (force w1)+w2
        | CompoundBranch (l1,w1), CompoundBranch (l2,w2) -> 
            (force w1)+(force w2)
;;
let rec balanced : mobile -> bool
= fun m -> 
    match m with 
        SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> 
            if (l1 * w1 = l2 * w2) then true else false
        | SimpleBranch (l1,w1), CompoundBranch (l2,w2) -> 
            if (balanced w2) && ((l1 * w1) = (l2 * (force w2))) then true else false
        | CompoundBranch (l1,w1), SimpleBranch (l2,w2) -> 
            if (balanced w1) && (l1*(force w1) = l2*w2) then true else false
        | CompoundBranch (l1,w1), CompoundBranch (l2,w2) -> 
            if (balanced w1)&&(balanced w2)&&(l1*(force w1) = l2*(force w2)) then true else false
;;