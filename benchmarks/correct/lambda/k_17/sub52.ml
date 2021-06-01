(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type fvset = var list
let extendfv v l = v::l
let rec xcludefv v l = match l with
  |[]->[]
  |hd::tl-> if hd=v then xcludefv v tl else hd::xcludefv v tl

let rec makefvset : lambda -> fvset -> fvset
= fun lam s -> match lam with
  |V v->extendfv v s
  |P(v,l)->xcludefv v (makefvset l s)
  |C(l1,l2)->(makefvset l2 s)@(makefvset l1 s)

let isempty l = match l with
  |[]->true
  |h::t->false

let rec check : lambda -> bool
= fun lam -> isempty (makefvset lam [])
