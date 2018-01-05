module type ZEXPR =
  sig
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
    type environment
    type value 
    val emptyEnv: environment
    val eval: environment * expr -> value
    val int_of_value : value -> int
  end

module Zexpr : ZEXPR=
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
    type value = VAL of int
    type environment = (id * value) list
    let emptyEnv = []

    let int_of_value : value -> int =
    fun v ->
      match v with
      | VAL n -> n
  
    let rec getMax : value list -> value =
    fun lst ->
      match lst with
      | [] -> VAL 0
      | a::[] -> a
      | _ ->
	let l::rest = lst in
	let rMax = getMax rest in
	if (int_of_value l) > (int_of_value rMax) then l
	else rMax
    
    let rec eval : environment * expr -> value = 
    fun params ->
      let (env, exp) = params in
      match exp with
      | NUM n -> VAL n
      | PLUS (e1, e2) -> VAL ((int_of_value (eval (env,e1))) + (int_of_value (eval (env,e2))))
      | MINUS (e1, e2) -> VAL ((int_of_value (eval (env,e1))) - (int_of_value (eval (env,e2))))
      | MULT (e1, e2) -> VAL ((int_of_value (eval (env,e1))) * (int_of_value (eval (env,e2))))
      | DIVIDE (e1, e2) -> VAL ((int_of_value (eval (env,e1))) / (int_of_value (eval (env,e2))))
      | MAX lst -> getMax (List.map (fun expr -> eval (env, expr)) lst)
      | VAR id ->
        (
        try
          (snd (List.find (fun x -> (fst x) = id) env))
	with Not_found -> (raise (Error "FreeVariable"))
	)
      | LET (id, e1, e2) -> (eval (((id, (eval (env, e1)))::env), e2))
  end 
