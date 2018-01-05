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

  val emptyEnv: environment
  val eval: environment * expr -> value

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

  type environment = (id, int) Hashtbl.t
  type value = int

  let emptyEnv : environment = Hashtbl.create 15

  let rec eval ((env : environment), (exp : expr)) : value =
    match exp with
    | NUM(i) -> i
    | PLUS(exp1, exp2) -> eval(env, exp1) + eval(env, exp2)
    | MINUS(exp1, exp2) -> eval(env, exp1) - eval(env, exp2)
    | MULT(exp1, exp2) -> eval(env, exp1) * eval(env, exp2)
    | DIVIDE(exp1, exp2) -> eval(env, exp1) / eval(env, exp2)
    | MAX(li) -> (
        let rec findmax((expli : expr list), (maxval : value)) : value =
          match expli with
          | [] -> maxval
          | hd::tl -> let newval = eval(env, hd) in
            let newmax = if (newval > maxval) then newval else maxval in
            findmax(tl, newmax)
        in
        match li with
        | [] -> 0
        | hd::tl -> findmax(tl, eval(env, hd))
      )
    | VAR(x) -> (
      try Hashtbl.find env x with
      Not_found -> raise (Error "FreeVariable")
    )
    | LET(newid, expfornewid, exp) -> (
      let valfornewid = eval(env, expfornewid) in
      let _ = Hashtbl.add env newid valfornewid in
      eval(env, exp)
    )

  let print_value (inputval : value) : unit =
    print_endline(string_of_int(inputval))
end