type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec isin ((str : string), (lst : string list)) : bool =
  match lst with
  | [] -> false
  | hd :: tl -> if hd = str then true else isin (str, tl)


let rec filter (lst : string list) (lst2 : string list) : bool =
  match lst with
  | [] -> true
  | hd :: tl -> if isin (hd, lst2) = true then filter tl lst2 else false


let rec bool_check ((lambda : lambda), (lst : string list)) : var list =
  match lambda with
  | V str -> lst
  | P (e1, e2) -> bool_check (e2, [ e1 ] @ lst)
  | C (e1, e2) -> bool_check (e1, lst) @ bool_check (e2, lst)


let rec cal_check ((lambda : lambda), (lst : string list)) : var list =
  match lambda with
  | V str -> str :: lst
  | P (e1, e2) -> cal_check (e2, lst)
  | C (e1, e2) -> cal_check (e1, lst) @ cal_check (e2, lst)


let check (lambda : lambda) : bool =
  if filter (cal_check (lambda, [])) (bool_check (lambda, [])) = true then true
  else false
