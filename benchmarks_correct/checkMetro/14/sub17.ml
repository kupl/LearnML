(* hw2-1 *)

type lambda =
    V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check m =
  let rec checkHelper g m =
    match m with
    | V s -> List.mem s g
    | P (a, n) -> checkHelper (a::g) n
    | C (n1, n2) -> (checkHelper g n1) && (checkHelper g n2)
  in
  checkHelper [] m
