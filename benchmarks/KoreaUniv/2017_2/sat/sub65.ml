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
= fun f ->
  let rec appendvar str l =
    match l with
    | [] -> str::l
    | h::t -> if h = str then l else h::(appendvar str t) in
  let rec findvar form l =
    match form with 
    | True
    | False -> l
    | Var str -> appendvar str l
    | Neg f1 -> findvar f1 l
    | And (f1, f2)
    | Or (f1, f2)
    | Imply (f1, f2)
    | Iff (f1, f2) -> findvar f1 (findvar f2 l) in
  let rec setvar form (s, b) =
    match form with 
    | True -> True
    | False -> False
    | Var str -> if str = s then b else (Var str)
    | Neg f1 -> Neg (setvar f1 (s, b))
    | And (f1, f2) -> And (setvar f1 (s, b), setvar f2 (s, b))
    | Or (f1, f2) -> Or (setvar f1 (s, b), setvar f2 (s, b))
    | Imply (f1, f2) -> Imply (setvar f1 (s, b), setvar f2 (s, b))
    | Iff (f1, f2) -> Iff (setvar f1 (s, b), setvar f2 (s, b)) in
  let rec eval form =
    match form with 
    | True -> true
    | False -> false
    | Var str -> raise (Failure "There is variable in formula")
    | Neg f1 -> not (eval f1)
    | And (f1, f2) -> (eval f1) && (eval f2)
    | Or (f1, f2) -> (eval f1) || (eval f2)
    | Imply (f1, f2) -> (not (eval f1)) || (eval f2)
    | Iff (f1, f2) -> (eval f1) = (eval f2) in
  let rec recursiveeval form l =
    match l with
    | [] -> eval form
    | h::t ->
        (recursiveeval (setvar form (h, True)) t)
        || (recursiveeval (setvar form (h, False)) t) in
  recursiveeval f (findvar f [])
