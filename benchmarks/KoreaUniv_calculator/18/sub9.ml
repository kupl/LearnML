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
  let rec calc_sigma : int -> int -> exp -> int
  = fun i n f -> if i <= n then 
    (match f with
    | X -> i
    | INT x -> x
    | ADD (e1,e2) -> (calc_sigma i i e1) + (calc_sigma i i e2)
    | SUB (e1,e2) -> (calc_sigma i i e1) - (calc_sigma i i e2)
    | MUL (e1,e2) -> (calc_sigma i i e1) * (calc_sigma i i e2)
    | DIV (e1,e2) -> if (calc_sigma i i e2) <> 0 then (calc_sigma i i e1) / (calc_sigma i i e2) else 0
    | SIGMA (e1,e2,e3) -> calc_sigma (calc_sigma i i e1) (calc_sigma i i e2) e3
   ) + calc_sigma (i+1) n f
    
    else 0
    
  in 
  match exp with
  | INT n -> n
  | ADD (e1,e2) -> (calculator e1) + (calculator e2)
  | SUB (e1,e2) -> (calculator e1) - (calculator e2)
  | MUL (e1,e2) -> (calculator e1) * (calculator e2)
  | DIV (e1,e2) -> if (calculator e2) <> 0 then (calculator e1) / (calculator e2)
                else 0
  | SIGMA (e1,e2,e3) -> calc_sigma (calculator e1) (calculator e2) e3
;;
