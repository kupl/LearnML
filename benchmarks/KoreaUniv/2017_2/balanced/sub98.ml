(*problem 6*)


type mobile = branch * branch
and branch = SimpleBranch of length * weight
			|CompoundBranch of length * mobile

and length = int
and weight = int

let node (l,w) = w



let rec scale bm = match bm with
|SimpleBranch(l,w)-> node(l,w)
|CompoundBranch(l,m)-> match m with
|(left,right) -> scale left + scale right


let rec balanced : mobile -> bool
= fun m -> match m with 
|(l,r)-> match l, r with
|(CompoundBranch(ln,mob),SimpleBranch(len,we))->if (ln* scale l)-(len *we) >0 ||(len *we) - (ln * scale l) >0 then false else true
|(CompoundBranch(ln,mob),CompoundBranch(lng,mo))->if (ln* scale l)-(lng*scale r) >0 ||(lng*scale r) - (ln * scale l) >0 then false else true
|(SimpleBranch(le,we),SimpleBranch(len,w))->if (le*we)-(len*w) >0 ||(len*w) - (le*we) >0 then false else true
|(SimpleBranch(le,we),CompoundBranch(lng,mo))->if (le*we)-(lng*scale r) >0 ||(lng*scale r) - (le*we) >0 then false else true
