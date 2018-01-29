
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;

  let rec calculator : exp -> int
  = fun exp -> 
let rec sigma (a,b,f) = if a=b then f a else (f a) + (sigma (a+1,b,f))
in let rec expf : exp -> (int -> int)
= fun expfun -> match expfun with
  X-> (fun x -> x)
| INT a -> (fun x -> a)
| ADD (a,b) -> (fun x -> ((expf a)x)+((expf b)x))
| SUB (a,b) -> (fun x -> ((expf a)x)-((expf b)x))
| MUL (a,b) -> (fun x -> ((expf a)x)*((expf b)x))
| DIV (a,b) -> (fun x -> ((expf a)x)/((expf b)x))
| SIGMA (a,b,f) -> (fun x -> sigma((expf a)x,(expf b)x,(expf f)))
in match exp with
  X -> raise (Failure "We cannot calculate just with it")
| INT a -> a
| ADD (a,b) -> (calculator a) + (calculator b)
| SUB (a,b) -> (calculator a) - (calculator b)
| MUL (a,b) -> (calculator a) * (calculator b)
| DIV (a,b) -> (calculator a) / (calculator b)
| SIGMA (a,b,f) -> sigma (calculator a, calculator b, expf f);; 	