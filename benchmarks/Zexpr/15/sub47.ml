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
 val print_value : value -> unit
end

module Zexpr : ZEXPR =
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
 type environment = (string*int) list
 type value = int

 let emptyEnv = []

 let rec finds : environment*string -> value = fun (en,s) ->
  match en with
  | [] -> raise (Error "FreeVariable")
  | (s1,i1)::l -> if s1=s then i1
   else finds(l,s)

 let rec eval = fun (en,e) ->
  match e with
  | NUM(i) -> i
  | PLUS(e1,e2) -> eval(en,e1)+eval(en,e2)
  | MINUS(e1,e2) -> eval(en,e1)-eval(en,e2)
  | MULT(e1,e2) -> eval(en,e1)*eval(en,e2)
  | DIVIDE(e1,e2) -> eval(en,e1)/eval(en,e2)
  | MAX el ->
   (match el with
   | [] -> 0
   | e1::[] -> eval(en,e1)
   | e1::l -> (max (eval(en,e1)) (eval(en,MAX l))))
  | VAR s -> finds(en,s)
  | LET(s,e1,e2) -> eval((s,eval(en,e1))::en,e2)

 let print_value v = (print_int v)
end 

