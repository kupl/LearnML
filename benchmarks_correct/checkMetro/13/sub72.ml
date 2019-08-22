type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string
let rec check met =
  let rec sset e set =
    match set with
    | [] -> false
    | hd::tl -> (e = hd) || (sset e tl)
  in
  let rec inarray f lst =
    match f with
    | V n -> (sset n lst)
    | P (n, m) -> (inarray m (n::lst))
    | C (m1, m2) -> (inarray m1 lst) && (inarray m2 lst)
  in
  inarray met [] 