  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string

let rec comblist = fun (l1,l2) -> match l1 with
| [] -> l2
| hd::tl -> hd::comblist(tl,l2)

let rec searchlist : (string * string list) -> bool
= fun (a, l) -> match l with
| [] -> false
| hd::tl -> if (hd = a) then true else searchlist(a, tl)

let rec complist : string list -> string list
= fun (l) -> match l with
| [] -> []
| hd::tl -> if (searchlist(hd, tl)) then complist(tl)
else hd::tl

let rec vars : lambda -> string list
=fun e -> match e with
| V x -> [x]
| P (x, e1) -> vars(e1)
| C (e1, e2) -> comblist(vars(e1),vars(e2))

let rec used : lambda -> string list
=fun e -> match e with
| V x -> []
| P (x, e1) -> x::used(e1)
| C (e1, e2) -> comblist(used(e1),used(e2))

let rec matchvar : (string list * string list) -> bool
=fun (vr, us) -> match vr,us with
| [],_ -> true
| hd::tl, us -> if (searchlist(hd, us)) then matchvar(tl, us)
else false
  
  let check : lambda -> bool
  =fun e -> matchvar(complist(vars(e)), used(e))
