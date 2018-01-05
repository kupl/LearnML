module type ZEXPR = sig 
  exception Error of string 
  type id = string 
  type expr = 
    | NUM of int 
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

module Zexpr:ZEXPR=
struct
	 exception Error of string 
  type id = string 
  type expr = 
    | NUM of int 
    | PLUS of expr * expr 
    | MINUS of expr * expr 
    | MULT of expr * expr 
    | DIVIDE of expr * expr 
    | MAX of expr list 
    | VAR of id 
    | LET of id * expr * expr 

  type environment = (id * int) list
  type value = expr

let rec list_max list =
      match list with
      | [] -> 
        raise (Error "empty list")
      | [x] -> 
         x
      | x :: remainder -> 
         max x (list_max remainder)

let int_of_value(value)=
	match value with
	| NUM (a) -> a
	| _ -> raise (Error "can't change to integer")

let emptyEnv =  ([])
let rec eval(env, expr) =
	match expr with
	| NUM (a) -> NUM (a)
	| PLUS (a, b) ->
		begin
			match (a, b) with
			| (NUM (num1), NUM (num2)) ->
				NUM (num1+num2)
			| (_, _) ->
				eval(env,PLUS(eval(env, a),eval(env,b)))
		end
	| MINUS (a, b) ->
		begin
			match (a,b) with
			| (NUM (num1), NUM (num2)) ->
				NUM (num1-num2)
			| (_,_) ->
				eval(env,MINUS(eval(env, a),eval(env,b)))
		end
	| MULT (a, b) ->
		begin
			match (a,b) with
			| (NUM (num1), NUM (num2)) ->
				NUM (num1*num2)
			| (_,_) ->
				eval(env,MULT(eval(env, a),eval(env,b)))
		end
	| DIVIDE (a, b) ->
		begin
			match (a,b) with
			| (NUM (num1), NUM (num2)) ->
				NUM (num1/num2)
			| (_,_) ->
				eval(env,DIVIDE(eval(env, a),eval(env,b)))
		end
	| MAX(list) ->
		if list = [] then NUM 0
		else 
				NUM (list_max (List.map (fun exp -> int_of_value(eval(emptyEnv,exp))) list))
	| VAR (id) ->
		if List.mem_assoc id env then NUM (List.assoc id env)
		else raise (Error "FreeVariable")
	| LET (id, expr1, expr2) ->
		eval(List.append [(id,int_of_value(eval(env,expr1)))] env, expr2)
end
