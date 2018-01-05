(* hw 2-6 *)
(* 2012-11269 DongJae Lim *)

module type ZEXPR = sig 
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

  val int_of_value : value -> int 
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
    type environment = (id * value) list

    let var ((env : environment), (i : id)) : value =
      let found = (List.filter (fun (i_, v) -> (i_ = i)) env) in
      match found with
      | [] -> raise (Error "FreeVariable")
      | (i_, v)::tl -> v

    let addenv ((env : environment), (i : id), (v : value)) : environment =
      let nfound = (List.filter (fun (i_, v) -> (i_ <> i)) env) in
      (i, v)::nfound

    let emptyEnv = []
    let rec eval ((env : environment), (e : expr)) : value =
      match e with
      | NUM (n) -> n
      | PLUS (e0, e1) -> (eval (env, e0)) + (eval (env, e1))
      | MINUS (e0, e1) -> (eval (env, e0)) - (eval (env, e1))
      | MULT (e0, e1) -> (eval (env, e0)) * (eval (env, e1))
      | DIVIDE (e0, e1) -> (eval (env, e0)) / (eval (env, e1))
      | MAX (l) -> 
        (match l with
         | [] -> 0
         | e0::_ ->
           let imax = (eval (env, e0)) in
           (List.fold_left
             (fun max ex -> 
               let t = (eval (env, ex)) in
               if t > max
               then t
               else max
             )
             imax l
           ))
      | VAR (i) -> (var (env, i))
      | LET (i, e0, e1) -> (eval ((addenv (env, i, (eval (env, e0)))), e1))

    let int_of_value (v : value) : int = v
  end
