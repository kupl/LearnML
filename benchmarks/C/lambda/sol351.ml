(* 컴퓨터공학과/2017-34165/김성국/2-4 *)
type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check m =
  let rec helper m lst =
    match m with
    | V n -> List.mem n lst
    | P(n, m1) -> helper m1 (n::lst)
    | C(m1, m2) -> (helper m1 lst) && (helper m2 lst)
  in
  helper m []
