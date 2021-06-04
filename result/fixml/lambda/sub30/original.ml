type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check2 (m, l) =
  match m with
  | P (nam, met) -> check2 (met, nam :: l)
  | V nam ->
      let x = List.find (fun elem -> elem = nam) l in
      true
  | C (met1, met2) -> check2 (met1, l) && check2 (met2, l)


let check m = check2 (m, [])
