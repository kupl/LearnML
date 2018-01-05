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
  type environment = (string -> value)
  let emptyEnv : environment = fun x -> (raise (Error "FreeVariable"))
	
  let rec eval : environment * expr -> value = fun (x, y) -> 
   match y with
   | NUM n -> n
   | PLUS (m, n) -> ((eval (x, m)) + (eval (x, n)))
   | MINUS (m, n) -> ((eval (x, m)) - (eval (x, n)))
   | MULT (m, n) -> ((eval (x, m)) * (eval (x, n)))
   | DIVIDE (m, n) -> ((eval (x, m)) / (eval (x, n)))
   | MAX l ->
    (match l with
	| [] -> 0
	| head::[] -> eval (x, head)
	| head::tail -> 
	 if (eval (x, head)) > (eval (x, MAX tail)) then eval (x, head)
	 else eval (x, MAX tail))
   | VAR name -> (x name)
   | LET (name, y1, y2) -> 
    let xp : (id * expr * environment) -> environment = fun (x, y, z) -> (fun w ->
	 if (w = x) then eval (z, y)
	 else z w) in
	eval (xp (name, y1, x), y2)
	
  let print_value : value -> unit = fun x -> print_endline (string_of_int x)
    
end 

 