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

    val emptyEnv: environment
    val eval: environment * expr -> value
      
    val print_value: value -> unit
  end

module Zexpr: ZEXPR =
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
   
    type environment = (id * int) list
    type value = int

    let emptyEnv = []
    let rec eval ((e: environment),(x: expr)) = match x with
      | NUM a -> a
      | PLUS (a, b) -> eval(e, a) + eval(e, b)
      | MINUS (a, b) -> eval(e, a) - eval(e, b)
      | MULT (a, b) -> eval(e, a) * eval(e, b)
      | DIVIDE (a, b) -> eval(e, a) / eval(e, b)
      | MAX elist -> (match elist with
        | [] -> 0
        | hd :: tl -> (let rec maxfind l m = match l with
            | [] -> m
            | hd :: tl -> let nm = eval(e, hd) in
              if (nm > m) then maxfind tl nm else maxfind tl m
           in maxfind elist (eval (e,hd))))
      | VAR a -> 
        let rec envcheck e b = match e with
            | [] -> raise (Error "FreeVariable")
            | (i, ex) :: tl -> if (i = b) then ex else envcheck tl b 
        in envcheck e a 
      | LET (i, a, b) -> eval((i, eval (e,a)) :: e, b)
        
    let print_value x = print_int x 
  end
