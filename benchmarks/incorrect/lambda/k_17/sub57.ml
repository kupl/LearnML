(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec varfind
=fun arr x->
match arr with
|[]->false
|hd::tl->if hd=x then true else varfind tl x

let rec find
=fun arr lam->
match lam with
|V x->(varfind arr x,arr)
|P(x,l)->let arr'=x::arr in find arr' l
|C(l1,l2)-> 
   let (b,arr')= (find arr l1) in let (b2,arr'')=(find arr' l2) in (b&&b2,arr'')
  
let rec check : lambda -> bool
= fun lam-> let (b,a)=find [] lam in b
