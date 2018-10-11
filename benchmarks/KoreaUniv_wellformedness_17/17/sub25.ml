(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda


type bounded = var list

let update_bounded a stk = a::stk;;
let rec find_bounded a stk = match stk with
|[]->false
|hd::tl->if hd=a then true else find_bounded a tl

let rec boundCheck : lambda -> bounded -> bool
= fun lam bd -> match lam with
|V v->find_bounded v bd
|P (v,l)-> let s = update_bounded v bd in boundCheck l s
|C (l1,l2)-> if boundCheck l2 bd then boundCheck l2 bd else false


let rec check : lambda -> bool
= fun lam -> boundCheck lam []




