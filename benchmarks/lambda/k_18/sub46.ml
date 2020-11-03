type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec elem a l =
  match l with
  | hd::tl -> (a = hd) || (elem a tl)
  | [] -> false

let rec _check lam l =
  match lam with
  | V v -> elem v l
  | P (v, lam') -> _check lam' (v::l)
  | C (lam1, lam2) -> (_check lam1 l) && (_check lam2 l)

let check : lambda -> bool
= fun lam -> _check lam []

;;