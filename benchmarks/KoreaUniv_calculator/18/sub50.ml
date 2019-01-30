type exp = 
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> 
    match exp with
      | X -> raise (Failure "Unbound X")
      | INT x' -> x'
      | SUB (x', y') -> calculator x' - calculator y'
      | ADD (x', y') -> calculator x' + calculator y'
      | DIV (x', y') -> calculator x' / calculator y'
      | MUL (x', y') -> calculator x' * calculator y'
      | SIGMA (x', y', expr') ->
      
   let rec cal_S min max exp result = 
        
    let rec exp_of_x x my_exp =
      match my_exp with
        | X -> x
        | INT a -> a
        | SUB (exp1, exp2) -> (exp_of_x x exp1) - (exp_of_x x exp2)
        | ADD (exp1, exp2) -> (exp_of_x x exp1) + (exp_of_x x exp2)
        | DIV (exp1, exp2) -> (exp_of_x x exp1) / (exp_of_x x exp2)
        | MUL (exp1, exp2) -> (exp_of_x x exp1) * (exp_of_x x exp2)
        | SIGMA(mine, maxe, expX) ->
          let min = exp_of_x x mine and max = exp_of_x x maxe in 
          cal_S min max expX 0
        in
        if  min<=max then cal_S (min+1) max exp (result + (exp_of_x min exp)) else result
        
        in cal_S (calculator x') (calculator y') expr' 0;;
        
        
        
        
        