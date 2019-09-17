type lambda = 
  V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check m =
  let rec checkName (str, l) =
    match l with
    | [] -> false
    | head::tail -> if str=head then true else checkName(str, tail)
  in
  let rec check2 (m, l) = 
    match m with
    | V a ->checkName(a, l)
    | P (a, b) -> check2(b, a::l)
    | C (a, b) -> (check2(a, l) && check2(b, l))
  in
  check2(m,[]);;
