type formula =
    | True
    | False
    | Not of formula
    | AndAlso of formula * formula
    | OrElse of formula * formula
    | Imply of formula * formula
    | Equal of exp * exp
and exp =
    | Num of int
    | Plus of exp * exp
    | Minus of exp * exp;;

let rec calc_exp (e: exp) : int =
    match e with
    | Num(i) -> i
    | Plus(ex1, ex2) -> ((calc_exp ex1) + (calc_exp ex2))
    | Minus(ex1, ex2) -> ((calc_exp ex1) - (calc_exp ex2));;

let rec eval (f: formula) : bool =
    match f with
    | True -> true
    | False -> false
    | Not(g) -> if eval g then false else true
    | AndAlso(g, h) -> let ge = eval g and he = eval h in
    if ge && he then true else false
    | OrElse(g, h) -> let ge = eval g and he = eval h in
    if ge || he then true else false
    | Imply(g, h) -> let ge = eval g and he = eval h in
    if ge && (not he) then false else true
    | Equal(e1, e2) -> let ev1 = calc_exp e1 and ev2 = calc_exp e2 in
    if ev1 = ev2 then true else false;;
