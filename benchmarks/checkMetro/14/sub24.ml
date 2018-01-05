type metro = 
  STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro m =
  let rec checkName (str, l) =
    match l with
    | [] -> false
    | head::tail -> if str=head then true else checkName(str, tail)
  in
  let rec checkMetro2 (m, l) = 
    match m with
    | STATION a ->checkName(a, l)
    | AREA (a, b) -> checkMetro2(b, a::l)
    | CONNECT (a, b) -> (checkMetro2(a, l) && checkMetro2(b, l))
  in
  checkMetro2(m,[]);;
