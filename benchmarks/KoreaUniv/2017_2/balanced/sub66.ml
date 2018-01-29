(*problem 6*)
type mobile=branch *branch
and branch=SimpleBranch of length*weight |CompoundBranch of length * mobile and length=int and weight= int;;

let rec part:branch->branch=fun e->
match e with
|CompoundBranch(a,(SimpleBranch(b1,c1),SimpleBranch(b2,c2)))->
  if b1*c1=b2*c2 then SimpleBranch (a,(c1+c2)) 
  else SimpleBranch (a,0)
|SimpleBranch(a,b)->SimpleBranch(a,b)
|CompoundBranch(a,(CompoundBranch(b,c),SimpleBranch(d,e)))->part (CompoundBranch(a,(part (CompoundBranch(b,c)),SimpleBranch(d,e))))
|CompoundBranch(a,(SimpleBranch(b,c),CompoundBranch(d,e)))->part (CompoundBranch(a,(SimpleBranch(b,c),part (CompoundBranch(d,e)))))
|CompoundBranch(a,(CompoundBranch(b,c),CompoundBranch(d,e)))->part (CompoundBranch(a,(part (CompoundBranch(b,c)),part (CompoundBranch(d,e)))))
let rec balanced: mobile->bool=fun m->
match m with
|(SimpleBranch(a,b),SimpleBranch(c,d))->if a*b <> 0 && a*b=c*d then true else false
|(CompoundBranch(a,b),SimpleBranch(c,d))->balanced (part (CompoundBranch(a,b)),SimpleBranch(c,d))
|(SimpleBranch(a,b),CompoundBranch(c,d))->balanced((SimpleBranch(a,b),part (CompoundBranch(c,d))))
|(CompoundBranch(a,b),CompoundBranch(c,d))->balanced((part (CompoundBranch(a,b)),part (CompoundBranch(c,d))))
