type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec listcheck : var -> var list -> bool =
 fun var varl ->
  match varl with
  | [] -> false
  | hd :: tl -> if var = hd then true else listcheck var tl


let rec lamfind : lambda -> var list -> bool =
 fun lam varl ->
  match lam with
  | V v -> listcheck v varl
  | P (v, lm) ->
      let vl = varl @ [ v ] in

      let film = lamfind lm vl in
      film
  | C (lm1, lm2) ->
      let film1 = lamfind lm1 varl in

      let film2 = lamfind lm2 varl in
      film1 && film2


let rec check : lambda -> bool =
 fun lam ->
  match lam with
  | V v -> false
  | P (v, lm) ->
      let chlm = lamfind lm [ v ] in
      chlm
  | C (lm1, lm2) -> false
