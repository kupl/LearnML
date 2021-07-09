exception Error of string

type lambda = V of string | P of (string * lambda) | C of (lambda * lambda)

let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (met : lambda) : bool =
  let rec check2 (a : string) (me : lambda) : bool =
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
  List.length (__s3 met) = 0
