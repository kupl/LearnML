exception Error of string

type lambda = V of string | P of (string * lambda) | C of (lambda * lambda)

let rec check met =
  let rec check2 a me =
    match me with
    | V x -> if x = a then true else false
    | P (q, V b) -> if a = b then true else false
    | P (x, C (t, p)) -> (
        match (t, p) with
        | V e, V f -> check2 x t || check2 x p
        | _ -> check2 x t && check2 x p )
    | P (x, P (e, f)) -> check2 x (P (e, f)) && check2 e f
    | C (x, y) -> (
        match (x, y) with
        | V e, V f -> check2 a x || check2 a y
        | _ -> check2 a x && check2 a x )
  in

  match met with P (b, c) -> check2 b c | _ -> raise Error "Illegal input"
