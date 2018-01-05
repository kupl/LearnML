(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-1 *)
type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

let rec compute : expr -> int = fun (e) ->
    match e with
    | NUM i -> i
    | PLUS (f,g) -> (compute (f) + compute (g))
    | MINUS (f,g) -> (compute (f) - compute (g))

let rec eval : formula -> bool = fun (a) ->
    match a with
    | TRUE -> true
    | FALSE -> false
    | NOT b -> 
        (match eval (b) with
         | true -> false
         | false -> true)
    | ANDALSO (b,c) -> 
        (match (eval (b), eval (c)) with
         | (true, true) -> true
         | _ -> false)
    | ORELSE (b,c) -> 
        (match (eval (b), eval (c)) with
         | (false, false) -> false
         | _ -> true)
    | IMPLY (b,c) ->
        (match (eval (b), eval (c)) with
         | (true, false) -> false
         | _ -> true)
    | LESS (e,f) -> (compute(e) < compute(f))
(* Test Code
let e : expr = PLUS (NUM 3, MINUS (NUM 8, NUM 4))
let f : expr = PLUS (NUM 2, NUM 6)
let g : expr = NUM 5
let x : formula = NOT (ORELSE (IMPLY(LESS(e,f), LESS(g, f)), ANDALSO(FALSE, TRUE)))
let y : formula = LESS(g, e)
let z : formula = IMPLY(x, y)
let w : formula = IMPLY(y, x)
let t : formula = ANDALSO(z, w)
let s : formula = ORELSE(w, z)

let test b = match eval (b) with
    | false -> print_endline ("false")
    | true -> print_endline ("true")

let _ = test (x)
let _ = test (y)
let _ = test (z)
let _ = test (w)
let _ = test (t)
let _ = test (s)
*)
