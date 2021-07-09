type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec listcheck (var : string) (varl : string list) : bool =
  match varl with
  | [] -> false
  | hd :: tl -> if var = hd then true else listcheck var tl


let rec lamfind (lam : lambda) (varl : string list) : bool =
  match lam with
  | V v -> listcheck v varl
  | P (v, lm) ->
      let vl : string list = varl @ [ v ] in

      let film : bool = lamfind lm vl in
      film
  | C (lm1, lm2) ->
      let film1 : bool = lamfind lm1 varl in

      let film2 : bool = lamfind lm2 varl in
      film1 && film2


let rec check (lam : lambda) : bool =
  match lam with
  | V v -> false
  | P (v, lm) ->
      let chlm : bool = lamfind lm [ v ] in
      chlm
  | C (lm1, lm2) -> check lm1 && check lm2
