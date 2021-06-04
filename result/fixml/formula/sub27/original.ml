type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec exp_to_int e =
  match e with
  | Num i -> i
  | Plus (e1, e2) -> exp_to_int e1 + exp_to_int e2
  | Minus (e1, e2) -> exp_to_int e1 - exp_to_int e2


let less_cmd (e1, e2) = if exp_to_int e1 = exp_to_int e2 then True else False

let and_also_cmd (f1, f2) =
  match (f1, f2) with True, True -> True | _ -> False


let or_else_cmd (f1, f2) =
  match (f1, f2) with False, False -> False | _ -> True


let imply_cmd (f1, f2) = match (f1, f2) with True, False -> False | _ -> True

let rec not_cmd f =
  match f with
  | True -> False
  | False -> True
  | Not innerF -> innerF
  | AndAlso (f1, f2) -> not_cmd (and_also_cmd (f1, f2))
  | OrElse (f1, f2) -> not_cmd (or_else_cmd (f1, f2))
  | Imply (f1, f2) -> not_cmd (imply_cmd (f1, f2))
  | Equal (e1, e2) -> not_cmd (less_cmd (e1, e2))


let rec formal_to_bool f =
  match f with
  | True -> true
  | False -> false
  | Not innerF -> formal_to_bool (not_cmd f)
  | AndAlso (f1, f2) -> formal_to_bool (and_also_cmd (f1, f2))
  | OrElse (f1, f2) -> formal_to_bool (or_else_cmd (f1, f2))
  | Imply (f1, f2) -> formal_to_bool (imply_cmd (f1, f2))
  | Equal (e1, e2) -> formal_to_bool (less_cmd (e1, e2))


let eval f =
  match f with
  | True -> true
  | False -> false
  | Not innerF -> formal_to_bool (not_cmd innerF)
  | AndAlso (f1, f2) -> formal_to_bool (and_also_cmd (f1, f2))
  | OrElse (f1, f2) -> formal_to_bool (or_else_cmd (f1, f2))
  | Imply (f1, f2) -> formal_to_bool (imply_cmd (f1, f2))
  | Equal (e1, e2) -> formal_to_bool (less_cmd (e1, e2))
