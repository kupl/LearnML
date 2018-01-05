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

  type environment = (id * int) list
  type value = int
  let emptyEnv = []
  type eval = environment * expr -> value

  type print_value = value -> unit
   (* Implement this module *)


   let rec varx ((l: environment), (x: id)) : int =
    match l with
    | [] -> raise (Error "FreeVariable")
    | hd::tl -> (match hd with
                |(a, b) -> if a=x then b else (varx (tl, x)))

   let rec eval ((a: environment), (b: expr)) : int =
    match b with
    | NUM x -> x
    | PLUS (x, y) -> (eval (a, x)) + (eval (a, y))
    | MINUS (x, y) -> (eval (a, x)) - (eval (a, y))
    | MULT (x, y) -> (eval (a, x)) * (eval (a, y))
    | DIVIDE (x, y) -> (eval (a, x)) / (eval (a, y))
    | MAX x ->
      (let rec maxofexprlist ((l : expr list), (r: int)) : int =
      match l with
      |[] -> r
      |hd::tl -> if eval (a, hd) > r then maxofexprlist(tl, (eval (a, hd)))
                else maxofexprlist(tl, r)
      in
      match x with
      | [] -> 0
      | hd::tl -> maxofexprlist (hd::tl, (eval (a, hd))))
    | VAR v -> varx (a, v)
    | LET (v,x,y) -> eval ([(v, eval (a, x))]@a, y)

    let print_value (f : int) : unit =
     print_endline (string_of_int f)
end
