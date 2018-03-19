(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> 
    let rec calc e cur =
      match e with
      |X -> cur
      |INT a -> a
      |ADD (a,b) -> (calc a cur) + (calc b cur)
      |SUB (a,b) -> (calc a cur) - (calc b cur)
      |MUL (a,b) -> (calc a cur) * (calc b cur)
      |DIV (a,b) -> (calc a cur) / (calc b cur)
      |SIGMA (a,b,c) -> let rec sigm bot =
                          if bot = calc b 0 then calc c bot
                          else (calc c bot) + sigm (bot+1)
                        in sigm (calc a 0)
    in calc e 0;;
