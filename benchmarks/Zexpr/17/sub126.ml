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
  type environment = (id * expr) list
  type value = int
let emptyEnv : environment = []
let rec eval : environment * expr -> value = fun  (v , e) -> 
  match  (v, e) with
  | (v, NUM k) -> k
  | (v, PLUS (x,y)) -> (eval (v,x)) + (eval (v, y))
  | (v, MINUS (x, y)) -> (eval (v,x)) - (eval (v, y))
  | (v, MULT (x, y)) -> (eval (v,x)) * (eval  (v, y))
  | (v, DIVIDE (x, y)) -> (eval (v, x)) / (eval  (v, y))
  | (v, MAX l) ->
    let comp : int -> int -> int = fun x y ->
      if x > y then -1
      else if x = y then 0
      else 1 in
    let rec change : environment * (expr list) -> ((environment * expr) list) = fun (v, l) ->  
      if l = [] then []
      else (v, List.hd l) :: change (v, List.tl l) in
    if l = [] then 0
    else List.hd (List.sort comp (List.map eval (change (v,l))))
  | (v, VAR s) -> 
    if List.mem s (List.map fst v ) then eval (v, (List.assoc s v)) 
    else raise (Error "FreeVariable")
  | (v, LET (s,y,z)) ->
    if List.mem s (List.map fst v) then eval (((s,y)::(List.remove_assoc s v)), z) 
    else eval(((s, y) :: v), z) 
let print_value : value -> unit = fun x -> print_endline (string_of_int x) 
end