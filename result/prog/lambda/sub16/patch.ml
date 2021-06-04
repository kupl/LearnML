type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 ((__s4 : lambda), (__s5 : string list)) : bool =
  match (__s4, __s5) with
  | V __s11, __s12 -> List.mem __s11 __s12
  | P (__s13, __s14), __s15 -> __s3 (__s14, List.append [ __s13 ] __s15)
  | C (__s16, __s17), __s18 -> __s3 (__s16, __s18) && __s3 (__s17, __s18)


let rec check (met : lambda) : bool =
  match met with
  | V n -> false
  | C (met1, met2) -> check met1 && check met2
  | P (stn0, met0) ->
      let check2 ((a : string), (b : string)) : bool =
        if a = b then true else false
      in

      let rec chstn ((stn : string), (met : lambda)) : bool =
        match met with
        | V stn' -> check2 (stn, stn')
        | C (n1, n2) -> chstn (stn, n1) || chstn (stn, n2)
        | P (st, n3) -> check2 (stn, st) || chstn (stn, n3)
      in
      __s3 (met0, [ stn0 ])
