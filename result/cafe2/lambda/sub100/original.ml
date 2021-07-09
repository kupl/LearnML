type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let check (lambda : lambda) : bool =
  let rec make_list (lambda' : lambda)
      ((lst1 : string list), (lst2 : string list)) : var list * var list =
    match lambda' with
    | V v -> (lst1, v :: lst2)
    | P (v, e) -> make_list e (v :: lst1, lst2)
    | C (e1, e2) -> make_list e1 (make_list e2 (lst1, lst2))
  in

  let rec exist (lst : string list) (v : string) : bool =
    match lst with
    | [] -> false
    | hd :: tl -> if hd = v then true else exist tl v
  in

  let rec real_check ((lst1 : string list), (lst2 : string list)) : bool =
    match lst2 with
    | [] -> true
    | hd :: tl -> if exist lst1 hd then real_check (lst1, tl) else false
  in
  real_check (make_list lambda ([], []))
