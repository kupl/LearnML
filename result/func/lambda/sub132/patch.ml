type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec erase (v : string) (lst : string list) : var list =
  match lst with
  | [] -> []
  | hd :: tl -> if v = hd then erase v tl else hd :: erase v tl


let rec check_help (lam : lambda) : var list =
  let lst : string list = [] in

  match lam with
  | V v -> v :: lst
  | P (v, l) -> erase v (check_help l)
  | C (l1, l2) -> check_help l1 @ check_help l2


let rec check (lam : lambda) : bool =
  match lam with
  | V v -> false
  | P (v, l) -> if List.length (erase v (check_help l)) = 0 then true else false
  | C (l1, l2) -> check l1 && check l2
