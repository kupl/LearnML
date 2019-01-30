type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp ->
  let rec calc exp x =
    match exp with
    | X -> x
    | INT(n) -> n
    | ADD(a,b) -> calc a x + calc b x
    | SUB(a,b) -> calc a x - calc b x
    | MUL(a,b) -> calc a x * calc b x
    | DIV(a,b) -> calc a x / calc b x
    | SIGMA(a,b,exp) ->  
      let ca = calc a x in
      let cb = calc b x in
      if ca > cb then 0 else calc exp ca + calc (SIGMA(INT(ca + 1) , INT(cb), exp)) ca in
  match exp with
    | X -> -10000
    | INT(n) -> n
    | ADD(x,y) -> calculator x + calculator y
    | SUB(x,y) -> calculator x - calculator y
    | MUL(x,y) -> calculator x * calculator y
    | DIV(x,y) -> calculator x / calculator y
    | SIGMA(a,b,exp) -> 
      let ca = calculator a in
      let cb = calculator b in
      if ca > cb then 0 else calc exp ca + calculator(SIGMA(INT(ca + 1), INT(cb), exp));;
