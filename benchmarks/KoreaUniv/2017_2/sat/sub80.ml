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
let rec to_DNF _f =
  match _f with
  | Neg (x) ->
  let impl = 
    match x with
    | True -> False
    | False -> True
    | Neg (y) -> to_DNF y
    | And (a, b) -> Or ((to_DNF (Neg a)), (to_DNF (Neg b)))
    | Or (a, b) -> And ((to_DNF (Neg a)), (to_DNF (Neg b)))
    | _ -> _f in
  impl
  | And (_x, _y) ->
  let impl =
    let x = to_DNF _x in
    let y = to_DNF _y in
    match x with
    | True -> y
    | False -> False
    | Or (a, b) -> Or ((to_DNF (And (a, y))), (to_DNF (And (b, y))))
    | _ ->
    let _impl =
      match y with
      | True -> x
      | False -> False
      | Or (a, b) -> Or ((to_DNF (And (x, a))), (to_DNF (And (x, b))))
      | _ -> And (x, y) in
    _impl in
  impl
  | Or (_x, _y) ->
  let impl =
    let x = to_DNF _x in
    let y = to_DNF _y in
    match x with
    | True -> True
    | False -> y
    | _ ->
    let _impl =
      match y with
      | True -> True
      | False -> x
      | _ -> Or (x, y) in
    _impl in
  impl
  | _ -> _f in
let rec to_NegAndOr _f =
  match _f with
  | Neg (x) -> Neg (to_NegAndOr x)
  | And (x, y) -> And ((to_NegAndOr x), (to_NegAndOr y))
  | Or (x, y) -> Or ((to_NegAndOr x), (to_NegAndOr y))
  | Imply (x, y) -> Or ((Neg (to_NegAndOr x)), (to_NegAndOr y))
  | Iff (x, y) ->
  let _x = to_NegAndOr x in
  let _y = to_NegAndOr y in
  And ((Or ((Neg _x), _y)), (Or ((Neg _y), _x)))
  | _ -> _f in
(to_DNF (to_NegAndOr f)) <> False;;
