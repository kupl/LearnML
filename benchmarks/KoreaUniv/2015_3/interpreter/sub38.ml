  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | LETREC of var * var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string

  type value = Int of int | Bool of bool 
             | Procedure of var * exp * env 
             | RecProcedure of var * var * exp * env
  and env = var -> value
  
  let empty_env = fun _ -> raise (Failure "Environment is empty")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x

let rec calc : program -> env -> value
=fun pgm e ->
match pgm with
|CONST i-> Int i
| VAR x-> apply_env e x
| ADD (a,b)-> (match calc a e,calc b e with
		|Int c,Int d-> Int(c + d)
		|_->raise (Failure "Type Error"))
| SUB (a,b)-> (match calc a e, calc b e with
		|Int c,Int d-> Int(c - d)
		|_->raise (Failure "Type Error"))
| ISZERO a-> if ((calc a e)=Int 0) then Bool true else Bool false
| IF (a,b,c)-> if (calc a e)=Bool true then (calc b e) else (calc c e)
| LET (x,a,b)-> calc b (extend_env (x,calc a e) e)
| LETREC (f,x,a,b)-> calc b (extend_env ( f, RecProcedure(f,x,a,e) ) e)
| PROC (x,a)-> Procedure(x,a,e)
| CALL (e1,e2)->
	(match calc e1 e with
	|Procedure(x,a,e0)-> calc a (extend_env (x,(calc e2 e)) e0)	
	|RecProcedure(f,x,a,e0)-> 
   calc a ( extend_env (x,(calc e2 e)) (extend_env ( f,RecProcedure(f,x,a,e0) ) e0) )
	|_->raise (Failure "Type Error")
	)
 
  let run : program -> value
  =fun pgm -> calc pgm empty_env
