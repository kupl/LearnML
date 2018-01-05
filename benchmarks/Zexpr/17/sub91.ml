let maxx (x, y) = if(x>y) then x else y 

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

    type environment = ((string, int) Hashtbl.t)
    type value = int

    let emptyEnv = (Hashtbl.create 123456)
    let rec eval (ev, ex) = match ex with
        | NUM a -> a
        | PLUS (a, b) -> (eval (ev, a)) + (eval (ev, b)) 
        | MINUS (a, b) -> (eval (ev, a)) - (eval (ev, b)) 
        | MULT (a, b) -> (eval (ev, a)) * (eval (ev, b)) 
        | DIVIDE (a, b) -> (eval (ev, a)) / (eval (ev, b))
        | MAX ll -> (match ll with
            | [] -> 0
            | h::[] -> eval (ev, h)
            | h::tl -> maxx(eval (ev, h), eval (ev, MAX tl)))
        | VAR id -> if(Hashtbl.mem ev id) then (Hashtbl.find ev id) else raise(Error "FreeVariable")
        | LET (id, va, ep) -> (Hashtbl.add ev id (eval (ev, va))); eval ((ev), ep)

    let print_value x = (print_int x)
end 
