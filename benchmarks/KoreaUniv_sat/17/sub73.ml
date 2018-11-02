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

let rec sat : formula -> bool
= fun f -> 
    match f with
    | True -> true
    | False -> false
    | Var str -> true
    | Neg form ->
            (match (sat form) with
            | true -> false
            | false -> true)
    | And (form1, form2) -> (sat form1) && (sat form2)
    | Or (form1, form2) -> (sat form1) || (sat form2)
    | Imply (form1, form2) -> 
            (match (sat form1) with
            | true -> 
                    (match (sat form2) with
                    | true -> true
                    | false -> false)
            | false -> true)
    | Iff (form1, form2) -> if (sat form1) = (sat form2) then true else false;;
