(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 2 - 6 *)
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

module Zexpr =
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
		type environment = (string * int) list
		type value = expr
		
		let emptyEnv : environment = []
		
		let int_of_value : value -> int = 
			function val_ ->
			match val_ with
			| NUM n -> n
			| otherexp -> raise (Error "WrongSyntax")
		
		let rec eval : environment * expr -> value =
			function (env, exp) ->
			match exp with
			| NUM n -> NUM n
			| PLUS (a, b) -> (
				let a = eval(env, a) in let b = eval(env, b) in
					match (a, b) with
					| (NUM aa, NUM bb) -> NUM (aa + bb)
					| (_, _) -> raise (Error "Illegal Expression"))
			| MINUS (a, b) -> (
				let a = eval(env, a) in let b = eval(env, b) in
					match (a, b) with
					| (NUM aa, NUM bb) -> NUM (aa - bb)
					| (_, _) -> raise (Error "Illegal Expression"))
			| MULT (a, b) -> (
				let a = eval(env, a) in let b = eval(env, b) in
					match (a, b) with
					| (NUM aa, NUM bb) -> NUM (aa * bb)
					| (_, _) -> raise (Error "Illegal Expression"))
			| DIVIDE (a, b) -> (
				let a = eval(env, a) in let b = eval(env, b) in
					match (a, b) with
					| (NUM aa, NUM bb) -> NUM (aa / bb)
					| (_, _) -> raise (Error "Illegal Expression"))
			| MAX lis -> (
				match lis with
				| [] -> NUM 0
				| elem::maxlist -> (
					let rec getmax = function lis ->
						match lis with
						| [] -> NUM min_int
						| e::l -> let a = int_of_value (eval(env,e)) in let b = int_of_value (getmax(l)) in
							if (a > b) then NUM a else NUM b in
						getmax(lis)))
			| VAR x -> (
				let rec search = function lis ->
					match lis with
					| [] -> raise (Error "FreeVariable")
					| (name,num)::envlist -> if (x = name) then NUM num else search envlist in
					search env)
			| LET (name, a, b) -> eval((name, int_of_value (eval(env, a)))::env, b)

	end