type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec checkSt_in_Metro ((var : string), (lambda : lambda)) : bool =
  match lambda with
  | V nm -> if nm = var then true else false
  | P (nm, met) -> checkSt_in_Metro (var, met)
  | C (met1, met2) ->
      checkSt_in_Metro (var, met1) || checkSt_in_Metro (var, met2)


let rec makeMetro_wo_St ((var : string), (lambda : lambda)) : lambda =
  match lambda with
  | V nm -> if nm = var then V "X" else V nm
  | P (nm, met) -> P (nm, makeMetro_wo_St (var, met))
  | C (met1, met2) ->
      C (makeMetro_wo_St (var, met1), makeMetro_wo_St (var, met2))


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (m : lambda) : bool = List.length (__s3 m) = 0
