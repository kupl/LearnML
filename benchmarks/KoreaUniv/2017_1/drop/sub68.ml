exception Problem

(*problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n->
if n>0 then 
match l with
| [] -> drop l (n-1)
| hd::tl -> drop tl (n-1)
else if n<0 then raise Problem
else l