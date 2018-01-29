(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int;;

let rec find_weight x =
match x with
CompoundBranch(a,(b,c)) -> (find_weight b) + (find_weight c)
| SimpleBranch(a,b) -> b;;

 let bal x =
match x with
| (CompoundBranch(a,(b,c)),CompoundBranch(d,(e,f))) -> if a*((find_weight b)+(find_weight c)) = d*((find_weight e)+(find_weight f)) then true else false
| (CompoundBranch(a,(b,c)),SimpleBranch(d,e)) -> if a*((find_weight b)+(find_weight c)) = d*e then true else false
| (SimpleBranch(a,b),CompoundBranch(c,(d,e))) -> if a*b = c*((find_weight d)+(find_weight e)) then true else false
| (SimpleBranch(a,b),SimpleBranch(c,d)) -> if a*b = c*d then true else false;;

let real x =
	match x with
| (CompoundBranch(a,(b,c)),CompoundBranch(d,(e,f))) -> if bal (b,c) && bal (e,f) then true else false
| (CompoundBranch(a,(b,c)),SimpleBranch(d,e)) -> if bal (b,c) then true else false
| (SimpleBranch(a,b),CompoundBranch(c,(d,e))) -> if bal (d,e) then true else false
| (SimpleBranch(a,b),SimpleBranch(c,d)) -> true;;

let balanced : mobile -> bool
= fun m -> if bal m && real m then true else false;;