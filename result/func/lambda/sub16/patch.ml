type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 ((__s4 : lambda), (__s5 : string list)) : bool =
  match __s4 with
  | V __s13 -> (
      match __s5 with
      | [] -> false
      | __s14 :: __s15 -> if __s14 = __s13 then true else __s3 (V __s13, __s15)
      )
  | P (__s16, __s17) -> __s3 (__s17, __s16 :: __s5)
  | C (__s18, __s19) -> __s3 (__s18, __s5) && __s3 (__s19, __s5)


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
