module type ZEXPR =
        sig
                exception Error of string
                type id = string
                type expr = NUM of int | PLUS of expr * expr
                        | MINUS of expr * expr | MULT of expr * expr
                        | DIVIDE of expr * expr | MAX of expr list
                        | VAR of id | LET of id * expr * expr
                type environment
                type value
                val emptyEnv: environment
                val eval: environment * expr -> value
                val print_value : value -> unit
        end

module Zexpr : ZEXPR =
        struct
                exception Error of string
                type id = string
                type expr = NUM of int | PLUS of expr * expr
                        | MINUS of expr * expr | MULT of expr * expr
                        | DIVIDE of expr * expr | MAX of expr list
                        | VAR of id | LET of id * expr * expr
		type value=int
                type envir_pair = (id*value)
                type environment = envir_pair list
                let emptyEnv: environment = []
                let print_value : value->unit  =fun i->
                        print_endline(string_of_int i)
                let rec eval: environment * expr -> value = fun (env,ex) ->
                        match ex with
                        | NUM(i) -> i
                        | PLUS(e1,e2) -> eval(env,e1)+eval(env,e2)
                        | MINUS(e1,e2) -> eval(env,e1)-eval(env,e2)
                        | MULT(e1,e2) -> eval(env,e1)*eval(env,e2)
                        | DIVIDE(e1,e2) -> eval(env,e1)/eval(env,e2)
                        | MAX(exList) ->( match exList with
                                | [] -> 0
                                | head::tail -> let a1=eval(env,head) in
					if(tail=[]) then a1
					else let a2=eval(env,MAX(tail)) in
					if (a1>a2) then a1 else a2
                                )
                        | VAR(i) -> let f:envir_pair->bool = fun (j,k)->i=j in
                                let i_env = List.filter f env in
                                (match i_env with
                                | [] -> raise (Error "FreeVariable")
                                | head::tail -> snd head)
                        | LET(i,e1,e2) -> let value = eval(env,e1) in
                                        let new_env=(i,value)::env in
                                        eval(new_env,e2)
        end

(*let a0=Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv,(Zexpr.LET("x",Zexpr.NUM 1, Zexpr.LET("y",Zexpr.NUM 3, Zexpr.PLUS(Zexpr.LET("x", Zexpr.VAR "y", Zexpr.LET("y", Zexpr.PLUS(Zexpr.VAR "x", Zexpr.NUM 2), Zexpr.MULT(Zexpr.VAR "x",Zexpr.VAR "y"))), Zexpr.DIVIDE(Zexpr.MULT(Zexpr.VAR "x",Zexpr.NUM 2), Zexpr.NUM 2)))))))
let a1=Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.NUM 1)))
let xpx = Zexpr.PLUS((Zexpr.VAR "x"),(Zexpr.VAR "x"))
let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x"))))
let a2=Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, e1))
let a3=Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [])))
let a4=Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)])))
let a5=Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv,Zexpr.LET("x",Zexpr.NUM 1,Zexpr.PLUS (Zexpr.LET("x",Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "x")),Zexpr.VAR "x"))))
let a6=Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv,Zexpr.LET("x",Zexpr.NUM 1,Zexpr.PLUS (Zexpr.LET("y",Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "y")),Zexpr.VAR "x"))))
let a7=Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv,Zexpr.LET("x",Zexpr.NUM 1,Zexpr.PLUS (Zexpr.LET("y",Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "y", Zexpr.VAR "x")),Zexpr.VAR "y"))))
*)
