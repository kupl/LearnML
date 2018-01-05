module type ZEXPR = 
sig
exception Error of string
type id = string
type expr = 
|NUM of int
|PLUS of expr * expr
|MINUS of expr * expr
|MULT of expr*expr
|DIVIDE of expr * expr
|MAX of expr list
|VAR of id
|LET of id*expr*expr

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
|NUM of int
|PLUS of expr * expr
|MINUS of expr * expr
|MULT of expr*expr
|DIVIDE of expr * expr
|MAX of expr list
|VAR of id
|LET of id*expr*expr

type environment = (string * int) list
type value = int

let emptyEnv = []

let newEnv ((en : environment),(i : id),(v : value)) : environment = 
if (List.mem_assoc i en) then (i,v)::(List.remove_assoc i en)
else (i,v)::en


let findId ((en : environment),(iid : id)) : int = 

if List.mem_assoc iid en then List.assoc iid en
else raise (Error "FreeVariable")

let rec exp2intList ((en : environment),(exlst : expr list)) : int list = 
match (en,exlst) with
|(en1,[]) -> []
|(en1,hd::tail) -> eval(en1,hd) :: exp2intList(en1,tail) 

and

eval ((en : environment),(exp : expr)) : value = 
match (en,exp) with
|(_,NUM n) -> n
|(env,PLUS(ex1,ex2)) -> eval(env,ex1) + eval(env,ex2)
|(env,MINUS(ex1,ex2)) -> eval(env,ex1) - eval(env,ex2)
|(env,MULT(ex1,ex2)) -> eval(env,ex1) * eval(env, ex2)
|(env,DIVIDE(ex1,ex2)) -> eval(env,ex1)/eval(env,ex2)
|(env,MAX([])) -> 0
|(env,MAX(elst)) ->List.hd(List.rev(List.sort compare (exp2intList(env,elst))))
|(env,VAR(iid)) -> findId(env,iid)
|(env,LET(iid,ex1,ex2)) -> eval(newEnv(env,iid,eval(env,ex1)),ex2)





 
let print_value (v : value)  : unit = print_int(v)
end
