(*problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec wei:mobile->int
=fun m->match m with
(SimpleBranch(x,y),SimpleBranch(a,b))->y+b
|(SimpleBranch(x,y),CompoundBranch(a,b))->y+wei(b)
|(CompoundBranch(x,y),SimpleBranch(a,b))->wei(y)+b
|(CompoundBranch(x,y),CompoundBranch(a,b))->wei(y)+wei(b);;

let rec balanced : mobile -> bool
= fun m -> (* TODO *)
match m with
(SimpleBranch(x,y),SimpleBranch(a,b))->(x*y=a*b)
|(SimpleBranch(x,y),CompoundBranch(a,b))->(x*y=wei(b)*a)&&(balanced b)
|(CompoundBranch(x,y),SimpleBranch(a,b))->(balanced y)&&(x*wei(y)=a*b)
|(CompoundBranch(x,y),CompoundBranch(a,b))->((x*wei(y))=(a*wei(b)))&&(balanced y)&&(balanced b);;