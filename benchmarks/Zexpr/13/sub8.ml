(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

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

    type value
    type environment

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

    type value = expr
    type environment = (id * value) list

    let emptyEnv = []

    let (>>>) a b = match (a,b) with
			(NUM(p),NUM(q)) -> if p > q then true else false
			| _ -> raise (Error "I don't know");;

    let rec eval ((envi:environment), (exp:expr)) = 
			let rec evali(env,e) = 
			(match e with
			  NUM(p) -> p
			| PLUS(p,q) -> evali(env,p) + evali(env,q)
			| MINUS(p,q) -> evali(env,p) - evali(env,q)
			| MULT(p,q) -> evali(env,p) * evali(env,q)
			| DIVIDE(p,q) -> evali(env,p) / evali(env,q)
			| MAX(lst) -> if lst = [] then raise (Error "MAX on Empty list")
					else 
						let max = ref (evali(env, List.hd(lst))) in
						for i = 1 to (List.length(lst) -1) do
						begin
							let cpr = evali(env, (List.nth lst i)) in
								if (cpr > !max) then max := cpr
						end done;
						!max
			| VAR(x) -> if(List.exists (fun s -> fst s = x) env) then evali(env, snd(List.find (fun s -> fst s = x) env))
					else raise (Error "FreeVariable")
			| LET(x,e1,e2) -> evali( ((x,NUM(evali(env,e1))) :: env), e2) ) in
			NUM(evali(envi,exp));;


    let rec int_of_value v = 
			match v with
			  NUM(p) -> p
			| PLUS(p,q) -> int_of_value(p) + int_of_value(q)
			| MINUS(p,q) -> int_of_value(p) - int_of_value(q)
			| MULT(p,q) -> int_of_value(p) * int_of_value(q)
			| DIVIDE(p,q) -> int_of_value(p) / int_of_value(q)
			|  _ -> int_of_value(eval(emptyEnv,v));;

end

