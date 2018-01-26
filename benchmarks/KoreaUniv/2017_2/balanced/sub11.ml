(*problem 6*)

type mobile =
branch *branch
and branch = SimpleBranch of length *weight
|CompoundBranch of length * mobile
and length = int
and weight = int;;
 
let rec baleq m =
match m with
|(SimpleBranch(ll,wl),SimpleBranch(lr,wr))->wl+wr
|(SimpleBranch(ll,wl),CompoundBranch(lr,mr))->(wl+(baleq mr))
|(CompoundBranch(ll,ml),SimpleBranch(lr,wr))->((baleq ml)+wr)
|(CompoundBranch(ll,ml),CompoundBranch(lr,mr))->((baleq ml)+(baleq mr));;

let rec balanced : mobile -> bool  = fun m->
match m with
|(SimpleBranch(a,b),SimpleBranch(c,d))->if ((a*b) =( c*d)) then true else false
|(SimpleBranch(a,b),CompoundBranch(c,d))->if (((a*b)=(c*baleq (d)))&&balanced d) then true else false
|(CompoundBranch(a,b),SimpleBranch(c,d))-> if (((c*d)=(a*baleq (b)))&&balanced b)then true else false
|(CompoundBranch(a,b),CompoundBranch(c,d))->if ((c*baleq d)=(a*baleq b)) && balanced b && balanced d then true else false;;
