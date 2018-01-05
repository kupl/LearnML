module type ZEXPR = 
sig 
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

  val print_value : value -> unit 
end 



module Zexpr : ZEXPR = 
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


  type value = int
  type environment = (string * int) list

  let rec getVal : environment * string -> int = fun(l, s) ->
	if(List.length l == 0) then raise(Error "no value")
	else if(fst(List.hd l ) = s) then snd(List.hd l)
	else getVal(List.tl l, s)

  let emptyEnv = []
  let rec eval : environment * expr -> value = fun(env, exp) ->
	match exp with
	| NUM i -> i
	| PLUS (e1,e2) -> eval(env, e1)+eval(env, e2)
	| MINUS (e1,e2) ->eval(env, e1)-eval(env, e2)
	| MULT (e1,e2) ->eval(env, e1)*eval(env, e2)
	| DIVIDE (e1,e2) ->eval(env, e1)/eval(env, e2)
	| MAX (el) ->
		if List.length el == 0 then 0
		else 
			let cur = eval(env, List.hd el) in
			if List.length el == 1 then cur
			else
				let max = eval(env, MAX(List.tl el)) in
				if max > cur then max
				else cur
	| VAR id -> getVal(env, id)
	| LET(id,e1,e2) -> eval((id, eval(env, e1))::env, e2)

  let print_value v = print_int(v)
end 



(*
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.NUM 1))) 

let xpx = Zexpr.PLUS((Zexpr.VAR "x"),(Zexpr.VAR "x")) 
let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x"))))
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, e1)) 

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX []))) 

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)])))
*) 


(*
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1,
                   Zexpr.PLUS (Zexpr.LET("x", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR
                   "x", Zexpr.VAR "x")),
                   Zexpr.VAR "x")
)))

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1,
                   Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM 2 , Zexpr.PLUS(Zexpr.VAR "x",
                   Zexpr.VAR "y")),
                                            Zexpr.VAR "x")
)))

let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, Zexpr.LET("x", Zexpr.NUM 1,
                   Zexpr.PLUS (Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR
                   "y", Zexpr.VAR "x")),
                                            Zexpr.VAR "y")
)))
*)