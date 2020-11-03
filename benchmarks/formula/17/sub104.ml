(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-1 *)
type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp

let rec compute : exp -> int = fun (e) ->
    match e with
    | Num i -> i
    | Plus (f,g) -> (compute (f) + compute (g))
    | Minus (f,g) -> (compute (f) - compute (g))

let rec eval : formula -> bool = fun (a) ->
    match a with
    | True -> true
    | False -> false
    | Not b -> 
        (match eval (b) with
         | true -> false
         | false -> true)
    | AndAlso (b,c) -> 
        (match (eval (b), eval (c)) with
         | (true, true) -> true
         | _ -> false)
    | OrElse (b,c) -> 
        (match (eval (b), eval (c)) with
         | (false, false) -> false
         | _ -> true)
    | Imply (b,c) ->
        (match (eval (b), eval (c)) with
         | (true, false) -> false
         | _ -> true)
    | Equal (e,f) -> (compute(e) = compute(f))
(* Test Code
let e : exp = Plus (Num 3, Minus (Num 8, Num 4))
let f : exp = Plus (Num 2, Num 6)
let g : exp = Num 5
let x : formula = Not (OrElse (Imply(Equal(e,f), Equal(g, f)), AndAlso(False, True)))
let y : formula = Equal(g, e)
let z : formula = Imply(x, y)
let w : formula = Imply(y, x)
let t : formula = AndAlso(z, w)
let s : formula = OrElse(w, z)

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
