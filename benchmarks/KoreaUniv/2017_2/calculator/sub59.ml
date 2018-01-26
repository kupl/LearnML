(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec new_fun : int -> exp -> int = fun i e ->
    match e with
    | X -> i
    | INT x -> x
    | ADD(e1, e2) -> new_fun i e1 + new_fun i e2
    | SUB(e1, e2) -> new_fun i e1 - new_fun i e2
    | MUL(e1, e2) -> new_fun i e1 * new_fun i e2
    | DIV(e1, e2) -> new_fun i e1 / new_fun i e2


let rec calculator : exp -> int
= fun e ->
    match e with
    | INT x -> x
    | ADD(e1, e2) -> calculator e1 + calculator e2
    | SUB(e1, e2) -> calculator e1 - calculator e2
    | MUL(e1, e2) -> calculator e1 * calculator e2
    | DIV(e1, e2) -> calculator e1 / calculator e2
    | SIGMA(srt,n,e) ->
        if (calculator srt) = (calculator n) then new_fun (calculator n) e
        else new_fun (calculator n) e + calculator (SIGMA(srt,SUB(n,INT 1),e))