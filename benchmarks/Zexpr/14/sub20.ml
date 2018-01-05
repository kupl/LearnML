module type ZEXPR = 
sig
  exception Error of string (* "FreeVariable" *)
  type id = string
  type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list
            | VAR of id
            | LET of id * expr * expr

  type environment
  type value

  val emptyEnv : environment
  val eval : environment * expr -> value
  val int_of_value : value -> int
end

module Zexpr : ZEXPR = 
struct
  exception Error of string
  type id = string
  type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list
            | VAR of id
            | LET of id * expr * expr
  type environment = (id * expr) list
  type value = expr
  let emptyEnv = []

  let rec max_list (maxElem: int) (l: int list): int =
    match l with
    | [] -> maxElem
    | h::t -> if h >= maxElem then (max_list h t)
              else max_list maxElem t

  let makePair (env: environment) (e: expr): (environment*expr) = 
    (env, e)

  let int_of_value (v: value): int = 
    match v with
    | NUM i -> i
    | _ -> raise (Error "FreeVariable")

  let rec eval: environment * expr -> value  =
    fun (env, e) ->
    match e with
    | NUM i -> NUM i

    | VAR v -> if env = [] then raise (Error "FreeVariable")
	       else let curr_env = List.hd env in
                    if (fst curr_env) = v then (snd curr_env) 
                    else VAR v

    | PLUS (e1, e2) -> (match (eval (env, e1)), (eval (env, e2)) with
		        | NUM i1, NUM i2 -> NUM (i1+i2)
		        | _ -> PLUS ( (eval (env, e1)), (eval (env, e2)) ))

    | MINUS (e1, e2) -> (match (eval (env, e1)), (eval (env, e2)) with
		         | NUM i1, NUM i2 -> NUM (i1-i2)
		         | _ -> MINUS ( (eval (env, e1)), (eval (env, e2)) ))

    | MULT (e1, e2) -> (match (eval (env, e1)), (eval (env, e2)) with
		        | NUM i1, NUM i2 -> NUM (i1*i2)
		        | _ -> MULT ( (eval (env, e1)), (eval (env, e2)) ))

    | DIVIDE (e1, e2) -> (match (eval (env, e1)), (eval (env, e2)) with
		          | NUM i1, NUM i2 -> NUM (i1/i2)
		          | _ -> DIVIDE ( (eval (env, e1)), (eval (env, e2)) ))

    | MAX eList -> if eList = [] then NUM 0
                   else let paired = List.map (makePair env) eList in
                        let evaled = List.map int_of_value (List.map eval paired) in
                        NUM (max_list (List.hd evaled) evaled)
                     
    | LET (id, e2, e3) -> let curr_env = [ (id, (eval (env, e2))) ] in
			  if env = [] then eval (curr_env, e3)
			  else eval (env, (eval (curr_env, e3)))
end

