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
  val print_value: value -> unit
 (* val string_of_value: value -> string *)
end

module Zexpr: ZEXPR =
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
    type value = int
    type environment = (id * value) list
    let emptyEnv = []
    let rec eval ((env:environment), (e:expr)) : value =
      let rec lookup (l: (string * int) list) (key: string): int =
        match l with
        | [] -> raise (Error "FreeVariable")
        | (k,v)::t -> if k=key then v else lookup t key
      in
      match e with
      | NUM i -> i
      | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
      | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
      | MULT (e1, e2) -> (eval (env, e1)) *  (eval (env, e2))
      | DIVIDE (e1, e2) -> let over = (eval (env, e2)) in if over = 0 
	                then raise (Error "Division by zero") else (eval (env, e1)) / over
      | MAX explist -> 
        let vallist = (List.map (fun x -> (eval (env, x))) explist) in
        let rec maxof l = match l with [] -> 0 | h::t -> 
 							if t != [] then (max h (maxof t)) else h in
        if explist = [] then 0 else maxof vallist
      | VAR id -> lookup env id
      | LET (id, e1, e2) -> eval (((id, eval (env, e1))::env), e2)
	let print_value (v:value) :unit =
	  print_endline (string_of_int v)
	
(*	let string_of_value v = string_of_int v*)
 end 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)])))
(*
	let e1 = Zexpr.LET("x", Zexpr.NUM 1,
	Zexpr.PLUS (Zexpr.LET("x", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "x")),
	Zexpr.VAR "x")
	)
	let e2 = Zexpr.LET("x", Zexpr.NUM 1,
	Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "y")),
	Zexpr.VAR "x")
	)
	let e3 = Zexpr.LET("x", Zexpr.NUM 1,
	Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "y", Zexpr.VAR "x")),
	Zexpr.VAR "y")
	)

	let f e = Zexpr.eval (Zexpr.emptyEnv, e)
*)	
