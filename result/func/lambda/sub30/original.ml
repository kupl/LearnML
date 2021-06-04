type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check2 ((m : lambda), (l : string list)) : bool =
  match m with
  | P (nam, met) -> check2 (met, nam :: l)
  | V nam ->
      let x : string = List.find (fun (elem : string) -> elem = nam) l in
      true
  | C (met1, met2) -> check2 (met1, l) && check2 (met2, l)


let check (m : lambda) : bool = check2 (m, [])
