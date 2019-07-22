type formula =
    | TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
and expr =
    | NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr;;

let rec calc_expr (e: expr) : int =
    match e with
    | NUM(i) -> i
    | PLUS(ex1, ex2) -> ((calc_expr ex1) + (calc_expr ex2))
    | MINUS(ex1, ex2) -> ((calc_expr ex1) - (calc_expr ex2));;

let rec eval (f: formula) : bool =
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT(g) -> if eval g then false else true
    | ANDALSO(g, h) -> let ge = eval g and he = eval h in
    if ge && he then true else false
    | ORELSE(g, h) -> let ge = eval g and he = eval h in
    if ge || he then true else false
    | IMPLY(g, h) -> let ge = eval g and he = eval h in
    if ge && (not he) then false else true
    | LESS(e1, e2) -> let ev1 = calc_expr e1 and ev2 = calc_expr e2 in
    if ev1 < ev2 then true else false;;
