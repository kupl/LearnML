(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int



let rec sum d =
match d with
|(CompoundBranch(a,b),CompoundBranch(x,y))->(sum b) + (sum y)
|(SimpleBranch(a,b),SimpleBranch(x,y))-> b + y
|(SimpleBranch(a,b),CompoundBranch(x,y))-> b + (sum y)
|(CompoundBranch(a,b),SimpleBranch(x,y))-> (sum b) + y

let rec balance tree
=match tree with
|(left,right)-> match (left, right) with 
                |(SimpleBranch (a,b), SimpleBranch(c,d)) -> a*b == c*d
                |(CompoundBranch(a,b), CompoundBranch(c,d))-> balance(SimpleBranch(a,(sum b)), SimpleBranch(c,sum d))
                |(SimpleBranch(a,b), CompoundBranch(c,d))->balance(SimpleBranch(a,b), SimpleBranch(c,sum d))
                |(CompoundBranch(a,b),SimpleBranch(c,d))-> balance(SimpleBranch(a ,(sum b)),SimpleBranch(c,d))



let balanced : mobile -> bool
= fun m -> balance m