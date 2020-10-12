type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec find : var -> var list -> bool
= fun x lst -> match lst with
  | [] -> false
  | hd :: tl -> if hd = x then true else find x tl

let check : lambda -> bool
= fun lam -> let rec checkrec : lambda -> var list -> bool
  = fun lam lst -> match lam with
    | V v -> find v lst
    | P (v, l) -> checkrec l (v :: lst)
    | C (l1, l2) -> checkrec l1 lst && checkrec l2 lst
  in checkrec lam [];;