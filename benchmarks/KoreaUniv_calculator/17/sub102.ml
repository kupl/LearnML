(* problem 5*)

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let result = ref 0         
let rec helpcalculator : exp -> int -> int
=fun e i ->
  match e with
  |INT a -> a
  |SUB (e1,e2)->
    let a1 = helpcalculator e1 i in
    let a2 = helpcalculator e2 i in
    a1-a2
  |MUL (e1,e2)-> 
    let a1 = helpcalculator e1 i in
    let a2 = helpcalculator e2 i in
    a1*a2
  |ADD (e1,e2)->
    let a1 = helpcalculator e1 i in
    let a2 = helpcalculator e2 i in
    a1+a2
  |DIV (e1,e2) ->
    let a1 = helpcalculator e1 i in
    let a2 = helpcalculator e2 i in
    a1/a2
  |X -> i
  |SIGMA (e1,e2,e3)->
    let a1 = (helpcalculator e1 i) in
    let a2 = (helpcalculator e2 i) in
    (match a2-a1 with
      |0 -> helpcalculator e3 a1
      |_ -> let qq = helpcalculator (SIGMA(INT (a1+1),INT a2,e3)) i in
            let rr = helpcalculator e3 a1 in
            qq+rr
    )

let rec calculator : exp -> int
= fun e -> 
  helpcalculator e 0