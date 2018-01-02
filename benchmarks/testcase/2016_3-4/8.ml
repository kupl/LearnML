type var = string

type exp =
	| V of var
	| P of var * exp
	| C of exp * exp


let rec has : string -> exp -> bool
  = fun str expression ->
    match expression with
    | V(v) -> false
    | P(v, e) ->
      if v = str then true else false
    | C(e1, e2) ->
      (has str e1) || (has str e2)
;;

let rec checkOriginal : exp -> exp -> bool
  = fun exp original ->
    match exp with
    | V(v) -> (has v original)
    | P(v, e) -> (checkOriginal e original)
    | C(e1, e2) -> (checkOriginal e1 original) && (checkOriginal e2 original)

let rec check : exp -> bool
  = fun exp ->
    checkOriginal exp exp
;;