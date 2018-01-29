(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

type value = N | B of bool
type env = (string * value) list

let empty_env = []

let extend_env (x, v) e = (x, v) :: e

let rec apply_env x e = match e with | [] -> N | (y, v)::tl -> if (x = y) then v else apply_env x tl

let rec eval : formula -> env -> value
= fun formula env -> match formula with | True -> B true | False -> B false | Var p -> apply_env p env | Neg p -> let v1 = eval p env in (match v1 with |N -> N |B true -> B false |B false -> B true) | And (p, q) -> if (Neg p = q) then B false else (if (Neg q = p) then B false else let v1 = eval p env in let v2 = eval q env in (match v1, v2 with | B true, B true -> B true | N, B true -> N | B true, N -> N | N, N -> N |_ -> B false))| Or (p, q) -> let v1 = eval p env in let v2 = eval q env in (match v1, v2 with |B false, B false -> B false | N,B false -> N | B false, N -> N | N, N -> N |_ -> B true) | Imply (p, q) -> let v1 = eval p env in let v2 = eval q env in(match v1, v2 with | B true, B false -> B false | B true, N -> N | N, B false -> N | N, N -> N |_ -> B true) | Iff (p, q) -> if (Neg p = q) then B false else (if (Neg q = p) then B false else if (p = q) then B true else let v1 = eval p env in let v2 = eval q env in (match v1, v2 with |B true,B false -> B false | B false, B true -> B false | B true, B true -> B true | B false, B false -> B true |_ -> N))


let sat : formula -> bool
= fun f -> match (eval f empty_env) with |N -> true | B true -> true | B false -> false