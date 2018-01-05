module type ZEXPR =
 sig
  exception Error of string
  type id = string
  type expr =
   |NUM of int
   |PLUS of expr * expr
   |MINUS of expr * expr
   |MULT of expr * expr
   |DIVIDE of expr * expr
   |MAX of expr list
   |VAR of id
   |LET of id * expr * expr

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
   |MULT of expr * expr
   |DIVIDE of expr * expr
   |MAX of expr list
   |VAR of id
   |LET of id * expr * expr
 
  type value = int
  type environment = (id * value) list
  
  let emptyEnv : environment = []
  let rec eval : environment * expr -> value = fun (en,ex) -> 
   match (en,ex) with
   | (en,NUM(num)) -> num
   | (en,PLUS(n1,n2)) -> eval(en,n1) + eval(en,n2)
   | (en,MINUS(n1,n2)) -> eval(en,n1) - eval(en,n2)
   | (en,MULT(n1,n2)) -> eval(en,n1) * eval(en,n2)
   | (en,DIVIDE(n1,n2)) -> eval(en,n1) / eval(en,n2)
   | (en,MAX(e1::e2::e)) -> if eval(en,e1)>eval(en,e2) then eval(en,MAX(e1::e)) else eval(en,MAX(e2::e))
   | (en,MAX(e::[])) -> eval(en,e)
   | (en,MAX([])) -> 0
   | ((idEn,valEn)::en,VAR(idVar)) -> if idEn=idVar then valEn else eval(en,VAR(idVar))
   | (emptyEnv,VAR(idVar)) -> raise (Error "FreeVariable")
   | (en,LET(id,ex1,ex2)) -> eval((id,eval(en,ex1))::en,ex2)

  let print_value: value -> unit = fun sth -> print_endline(string_of_int(sth))
 end

