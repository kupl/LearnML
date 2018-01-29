(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec scale
= fun bm ->
match bm with
|SimpleBranch(l,w)-> l*w 
|CompoundBranch(l,m)-> match m with
|(left,right) -> scale left + scale right

let balanced : mobile -> bool
= fun m ->
 match m with
|(l,r)-> match l, r with
|(CompoundBranch(ln,mob),SimpleBranch(len,we))->if (ln* scale l)-(scale r) >0 ||(scale r) - (ln * scale l) >0 then false else true
|(SimpleBranch(le,we),CompoundBranch(lng,mo))->if (scale l)-(lng*scale r) >0 ||(lng*scale r) - (scale l) >0 then false else true
|(CompoundBranch(ln,mob),CompoundBranch(lng,mo))->if (ln* scale l)-(lng*scale r) >0 ||(lng*scale r) - (ln * scale l) >0 then false else true
|(SimpleBranch(le,we),SimpleBranch(len,w))->if (scale l)-(scale r) >0 ||(scale r) - (scale l) >0 then false else true