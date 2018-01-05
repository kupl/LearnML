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
  
  val emptyEnv: environment
  val eval: environment * expr -> value

  val print_value: value -> unit
end

module Zexpr : ZEXPR = struct
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
  
  type environment = ENV of (id * int) list
  type value = VAL of int
  
  let emptyEnv = ENV([])
  let eval (env, expr) =
    let convert_to_value v =
      VAL v
    in
    let rec ev (en, ex) =
      match ex with
      | NUM i -> i
      | PLUS (e1, e2) -> ev (en, e1) + ev (en, e2)
      | MINUS (e1, e2) -> ev (en, e1) - ev (en, e2)
      | MULT (e1, e2) -> ev (en, e1) * ev (en, e2)
      | DIVIDE (e1, e2) -> ev (en, e1) / ev (en, e2)
      | MAX el ->
          let rec max lst =
            match lst with
            | [] -> 0
            | a::[] -> ev (en, a)
            | a::b::rest -> if (ev (en, a) > ev (en, b)) then max (a::rest) else max (b::rest)
          in
          max el
      | VAR id ->
          let rec findfirst (ENV lst) =
            match lst with
            | [] -> raise (Error "no such variable")
            | (x, v)::rest -> if x = id then v else findfirst (ENV rest)
          in
          findfirst en
      | LET (id, e1, e2) ->
          let newenv =
            match en with
            | ENV el -> ENV ((id, ev (en, e1))::el)
          in
          ev (newenv, e2)
    in
    convert_to_value (ev (env, expr))

  let print_value (VAL i) = print_int i; print_endline ""
end

(* TEST CASE *)
(*
let z1 = Zexpr.LET ("x", Zexpr.NUM 1, Zexpr.PLUS (Zexpr.LET ("x", Zexpr.NUM 2, Zexpr.PLUS (Zexpr.VAR "x", Zexpr.VAR "x")), Zexpr.VAR "x"));;
let z2 = Zexpr.LET ("x", Zexpr.NUM 1, Zexpr.PLUS (Zexpr.LET ("y", Zexpr.NUM 2, Zexpr.PLUS (Zexpr.VAR "x", Zexpr.VAR "y")), Zexpr.VAR "x"));;
let z3 = Zexpr.LET ("x", Zexpr.NUM 1, Zexpr.PLUS (Zexpr.LET ("y", Zexpr.NUM 2, Zexpr.PLUS (Zexpr.VAR "y", Zexpr.VAR "x")), Zexpr.VAR "y"));;
let z4 = Zexpr.LET ("x", Zexpr.NUM 1, Zexpr.PLUS (Zexpr.LET ("y", Zexpr.NUM 2, Zexpr.PLUS (Zexpr.MAX [Zexpr.VAR "x"; Zexpr.VAR "y"], Zexpr.VAR "x")), Zexpr.VAR "x"));;
let v1 = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, z1));;
let v2 = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, z2));;
(*let v3 = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, z3));;*)
let v4 = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, z4));;
*)
