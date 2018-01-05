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

module Zexpr : ZEXPR = struct
    
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

	type value = int
    type environment = (id * value) list

	let emptyEnv = []
   
	let env_id env = 					(* environment type�� ���ҿ��� id�� �����ش�. *)
		match env with
		| (id, num) -> id

	let env_num env = 					(* environment type�� ���ҿ��� int�� �����ش�. *)
		match env with
		| (id, num) -> num

	let rec find_rule (env, id) =		(* environment���� id���� ������ tuple�� ã�Ƽ� �����ش�. *)
		match env with
		| hd::tl -> if ((env_id hd) = id) then (env_num hd)
					else find_rule (tl, id)
		| [] -> raise (Error "FreeVariable")

	let rec env_add_rule ((env: environment), (id: id), (expr: value)) = 	(* environment�� ���ο� ��Ģ(tuple)�� �����ش�. ���� ���� ������ ���� ��Ģ�� �̹� �����ϸ� ������ �����. *)
		match env with
		| hd::tl -> if ((env_id hd) = id) then List.append [(id, expr)] tl
					else List.append [hd] (env_add_rule (tl, id, expr))
		| [] ->	[(id, expr)]
 
	let int_of_value v = v

    let rec eval ((env: environment), (e: expr)) = 
		match e with 
		| NUM i -> i
		| PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
		| MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
		| MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
		| DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
		| MAX elist -> find_max (elist)
		| VAR id -> find_rule(env, id)
		| LET (id, e1, e2) -> eval ((env_add_rule(env, id, (eval (env, e1)))), e2)		

	and find_max lst =																	(* list���� �ִ밪�� ã�Ƽ� �����ش�. *) 
		let rec expr_list_to_value lst = 												(* �ϴ� expr�� ��� value�� �ٲ��ش�. *)
			match lst with  
			| hd::tl -> List.append [(eval (emptyEnv, hd))] (expr_list_to_value tl)
			| [] -> []
		in 
		let rec find_max_value (lst, max_val) = 										(* value�� list�߿��� �ִ��� ã�´�. *)
			match lst with
			| hd::tl -> if (int_of_value (hd) > max_val) then find_max_value (tl, int_of_value (hd))
						else find_max_value (tl, max_val)
			| [] -> max_val
		in
		if lst = [] then 0
		else find_max_value ((expr_list_to_value lst), eval(emptyEnv, List.hd(lst)))
	end

