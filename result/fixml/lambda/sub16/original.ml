type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check met =
  match met with
  | V n -> true
  | C (met1, met2) -> check met1 && check met2
  | P (stn0, met0) ->
      let check2 (a, b) = if a = b then true else false in

      let rec chstn (stn, met) =
        match met with
        | V stn' -> check2 (stn, stn')
        | C (n1, n2) -> chstn (stn, n1) || chstn (stn, n2)
        | P (st, n3) -> check2 (stn, st) || chstn (stn, n3)
      in
      chstn (stn0, met0)
