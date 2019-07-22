type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string
let rec checkMetro met =
  let rec sset e set =
    match set with
    | [] -> false
    | hd::tl -> (e = hd) || (sset e tl)
  in
  let rec inarray f lst =
    match f with
    | STATION n -> (sset n lst)
    | AREA (n, m) -> (inarray m (n::lst))
    | CONNECT (m1, m2) -> (inarray m1 lst) && (inarray m2 lst)
  in
  inarray met [] 