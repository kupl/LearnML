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
  
let rec eval : exp*env -> value
=fun (exp,ev) -> match exp with
CONST n -> Int n
|VAR x -> apply_env ev x
|ADD (e1,e2) ->let a1= eval(e1,ev)
							in let a2=eval(e2,ev)
							in (match a1,a2 with
							|Int n1,Int n2 -> Int (n1+n2)
							|_ -> raise(Failure"error"))
|SUB (e1,e2) ->let a1=eval(e1,ev)
						in let a2=eval(e2,ev)
					in (match a1, a2 with
					|Int n1, Int n2 -> Int (n1-n2)
					|_-> raise(Failure "error"))
|ISZERO e ->let a=eval(e,ev) in (match a with
					|Int n -> if n=0 then Bool true else Bool false
					|_ -> raise(Failure "error"))
|IF (e1,e2,e3) ->let a=eval(e1,ev)
					in (match a with
					|Bool true -> eval(e2,ev)
					|Bool false -> eval(e3,ev)
					|_-> raise(Failure "error"))
|LET (x,e1,e2) -> let v1 = eval(e1,ev)
								in eval(e2, (extend_env(x,v1) ev))
|LETREC (f,x,e1,e2) ->eval (e2, (extend_env(f, RecProcedure(f,x,e1,ev)) ev))
|PROC (x,e) -> Procedure (x,e,ev)
|CALL (e1,e2) ->let p = eval(e1,ev)
							in (match p with
						|Procedure(x,e,ev1)->(
						let v1= eval(e2,ev)
						 in eval(e,(extend_env(x,v1) ev1)))
						|RecProcedure(f,x,e,ev1)-> let v=eval(e2,ev) in
eval(e,(extend_env (x,v) (extend_env(f,p) ev1)))
						|_-> raise(Failure "error"))

  let run : program -> value
  =fun pgm -> eval (pgm, empty_env)