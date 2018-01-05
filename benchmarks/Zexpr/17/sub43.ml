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

      type environment = (string * int) list
      type value = int

      let emptyEnv : environment = [] 
      let rec eval ((env : environment), (ex : expr)) : value =
          match ex with
              NUM i -> i
            | VAR id -> (match env with
                              (hd::tl) -> if ((fst hd) = id)
                                             then (snd hd)
                                             else (eval (tl, VAR id))
                            | [] -> raise (Error "FreeVariable"))
            | LET (id, ex1, ex2) -> eval ((id, eval (env, ex1)) :: env, ex2)
            | PLUS (ex1, ex2) -> (eval (env, ex1)) + (eval (env, ex2))
            | MINUS (ex1, ex2) -> (eval (env, ex1)) - (eval (env, ex2))
            | MULT (ex1, ex2) -> (eval (env, ex1)) * (eval (env, ex2))
            | DIVIDE (ex1, ex2) -> (eval (env, ex1)) / (eval (env, ex2))
            | MAX [] -> 0
            | MAX [ex] -> eval(env, ex)
            | MAX (hd::tl) -> max (eval (env, hd)) (eval (env, MAX tl))

      let print_value (v : value) : unit = 
          print_int v
    end 
