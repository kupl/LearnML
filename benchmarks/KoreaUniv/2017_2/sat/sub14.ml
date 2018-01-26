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

let sat : formula -> bool
= fun f -> (* TODO *)
  let rec helper f =
    match f with
    | False -> false
    | Neg (x) -> if (helper x = true) then false else true
    | And (Var x, Neg (Var y)) -> if x = y then false else true
    | Iff (Var x, Neg (Var y)) -> if x = y then false else true
    | And (x, y) -> helper x && helper y
    | Or (x, y) -> helper x || helper y
    | Imply (x, y) -> if ((helper x = true) && (helper y = false)) then false else true
    | Iff (x, y) -> if helper x = helper y then false else true
    | _ -> true in
      helper f

