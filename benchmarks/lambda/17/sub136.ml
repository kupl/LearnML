type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string


let check : lambda -> bool = function m ->
  let rec helper alist m  =
    match m with
    | V s -> List.mem s alist
    | P (n, m') -> helper (n::alist) m'
    | C (x, y) -> helper alist x && helper alist y
  in
  helper [] m
