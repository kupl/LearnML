module type ZEXPR = 
sig 
  exception Error_of_string 
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
 exception Error_of_string
 (*exception Not_found *)
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
 type value = int

 let emptyEnv = []

 let rec eval_rec env = function
   | NUM x -> x
   | VAR x -> List.assoc x env
   | LET (x, e1, in_e2) ->
    (* try *) 
       let val_x = eval_rec env e1 in
       eval_rec ((x,val_x)::env) in_e2  
  (*  with
       Not_found -> print_endline "FreeVariable" 
   *)
   | PLUS(x,y) ->
        (* try *)
                  let v1 = eval_rec env x in
                  let v2 = eval_rec env y in
                  v1 + v2
     (* with
       Error_of_string -> print_endline "FreeVariable" *)

   | MINUS(x,y) -> let v1 = eval_rec env x in
                  let v2 = eval_rec env y in
                  v1 - v2
   | MULT(x,y) -> let v1 = eval_rec env x in
                  let v2 = eval_rec env y in
                  v1 * v2
   | DIVIDE(x,y) -> let v1 = eval_rec env x in
                  let v2 = eval_rec env y in
                  if v2 = 0 then raise Error_of_string
                  else v1/v2
   | MAX x -> let rec list_max x = 
                   ( match x with
                     | [] -> 0
                     | [q] -> eval_rec env q
                     | x::tail ->  if eval_rec env x > eval_rec env (MAX tail) then eval_rec env x 
                                  else eval_rec env (MAX tail)
                    )
              in list_max(x)
            

 let eval = function(a,b) -> 
    eval_rec a b

 let print_value : value -> unit = fun a ->
      print_int a; print_endline""

   
end


(*
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.NUM 1))
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.PLUS(Zexpr.NUM 1, Zexpr.NUM 1)))
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.PLUS(Zexpr.PLUS(Zexpr.NUM 1,Zexpr.NUM 1),Zexpr.NUM 1)))
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [])))
let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr. LET("x",Zexpr.NUM  1, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "x") )    ))
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)])))

let xpx = Zexpr.PLUS((Zexpr.VAR "x"), (Zexpr.VAR "x"))
let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS(Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x")))) 
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, e1))

*)


let e2 = Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS(Zexpr.LET("x", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "x")), Zexpr.VAR "x") )
let e3 = Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS(Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "x", Zexpr.VAR "y")), Zexpr.VAR "x") )
let e4 = Zexpr.LET("x", Zexpr.NUM 1, Zexpr.PLUS(Zexpr.LET("y", Zexpr.NUM 2, Zexpr.PLUS(Zexpr.VAR "y", Zexpr.VAR "x")), Zexpr.VAR "y") )



let _ = Zexpr.print_value (Zexpr.eval(Zexpr.emptyEnv, e2))
let _ = Zexpr.print_value (Zexpr.eval(Zexpr.emptyEnv, e3))
(*
 let _ = Zexpr.print_value (Zexpr.eval(Zexpr.emptyEnv, e4))
*)

let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.DIVIDE(Zexpr.NUM 3,Zexpr.NUM 2)))





