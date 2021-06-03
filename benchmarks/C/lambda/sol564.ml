  (**********************)
  (*   Problem 2        *)
  (**********************)

  type lambda = V of var
              | P of var * lambda
              | C of lambda * lambda
              and var = string

let extend1: string-> string list->string list
= fun x l->  x::l
let rec find1: string-> string list-> bool
= fun x l->
match l with
|hd::tl-> if hd=x then true else find1 x tl
|_-> false

let rec hcheck : lambda -> string list -> bool
=fun lam s-> match lam with
|V x->find1 x s
|P (x, l)-> let s1=extend1 x s in hcheck l s1
|C (l1,l2)-> (hcheck l1 s) && (hcheck l2 s)



let rec check : lambda -> bool
 = fun lam -> hcheck lam [] 




