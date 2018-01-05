(* 프로그래밍언어 HW2 Exercise 6
   2009-11657 김동현 *)

(* environment의 용도? *)	
			
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
							
		type environment = ZEXPR of int list
		
		type value = VALIDV of int 
		           | INVALIDV
		
		let emptyEnv = ZEXPR []
		
		let eval (env, exp) =
			let rec searchPairList (elem, lst) =
			  match lst with
			  | [] -> raise (Error "Invalid variable") (* 변수가 정의되어 있지 않은 경우 예외 처리 *)
			  | h::t -> let (v, n) = h in
			            if elem = v then n
								  else searchPairList(elem, t) in
			(* (변수, 변수값)의 리스트에서 변수를 찾아 변수값 리턴 *)
			let rec cal (env, exp, lst) =
			  match env with
			  | ZEXPR l -> (match exp with
			                | NUM n -> n
                      | PLUS (exp1, exp2) -> (cal (env, exp1, lst)) + (cal (env, exp2, lst)) 
                      | MINUS (exp1, exp2) -> (cal (env, exp1, lst)) - (cal (env, exp2, lst)) 
                      | MULT (exp1, exp2) -> (cal (env, exp1, lst)) * (cal (env, exp2, lst)) 
                      | DIVIDE (exp1, exp2) -> (cal (env, exp1, lst)) / (cal (env, exp2, lst)) 
			                | MAX [] -> 0   (* 빈 리스트의 경우 0 리턴? 예외? *)
                      | MAX [n] -> cal (env, n, lst) 
                      | MAX (h::t) -> if (cal (env, h, lst)) > (cal (env, (MAX t), lst)) then (cal (env, h, lst))
				                              else (cal (env, (MAX t), lst))        
											| VAR v -> searchPairList (v, lst)
			                | LET (id, exp1, exp2) -> (cal (env, exp2, (id, (cal (env, exp1, lst)))::lst)))
			  | _ -> raise (Error "Invalid environment") in 
			let value_of_int i =
			  match i with
			  | n -> (VALIDV n)
			  | _ -> INVALIDV in
			(value_of_int (cal (env, exp, [])))
			(* cal 함수가 계산한 int 결과값을 value 타입으로 변환 *)
		
		let int_of_value v =
			match v with
			| VALIDV i -> i
			| INVALIDV -> raise (Error "Invalid value")
		(* value 타입 값을 int 타입 값으로 변환 *)
	end
	
(* module ValidZexpr = (Zexpr: ZEXPR) *)