type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec checkSt_in_Metro (var, lambda) =
  match lambda with
  | V nm -> if nm = var then true else false
  | P (nm, met) -> checkSt_in_Metro (var, met)
  | C (met1, met2) ->
      checkSt_in_Metro (var, met1) || checkSt_in_Metro (var, met2)


let rec makeMetro_wo_St (var, lambda) =
  match lambda with
  | V nm -> if nm = var then V "X" else V nm
  | P (nm, met) -> P (nm, makeMetro_wo_St (var, met))
  | C (met1, met2) ->
      C (makeMetro_wo_St (var, met1), makeMetro_wo_St (var, met2))


let rec check m =
  match m with
  | V var -> if var = "X" then true else false
  | P (var, lambda) ->
      if checkSt_in_Metro (var, lambda) then
        check (makeMetro_wo_St (var, lambda))
      else check lambda
  | C (lambda1, lambda2) -> check lambda1 && check lambda2
