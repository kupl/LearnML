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
    type expr = NUM of int
              | PLUS of expr * expr
              | MINUS of expr * expr
              | MULT of expr * expr
              | DIVIDE of expr * expr
              | MAX of expr list
              | VAR of id
              | LET of id * expr * expr
    type value = int
    type kimjeongeun = id * value
    type environment = EMPTY (*kimjeongeun list*)
                      | LINK of kimjeongeun * environment (* LIST of (kim * env) :: environment  *)
    let emptyEnv = EMPTY
    let rec eval: environment * expr -> value = fun (en , ex) ->
      let rec findid: environment * id -> value = fun (en , i) ->
        match en with
        | EMPTY -> raise(Error "Invalid_Expression")
        | LINK (kim, enx) -> (
          match kim with
          | j, value -> (
            if (i = j) then value
            else findid(enx, i)
            )
          )
      in
      let rec findmax: environment * expr list -> value = fun (en, el) ->
        match el with
        | [] -> 0
        | [x] -> eval(en, x)
        | hd :: tl -> max (eval(en, hd)) (findmax(en, tl))
      in
      match ex with
      | NUM n -> n
      | PLUS (a, b) -> eval (en, a) + eval (en, b)
      | MINUS (a, b) -> eval (en, a) - eval (en, b)
      | MULT (a, b) -> eval (en, a) * eval (en, b)
      | DIVIDE (a, b) -> eval (en, a) / eval (en, b)
      | MAX elist -> findmax(en, elist)
      | VAR i -> findid (en, i)
      | LET (i, a, b) -> eval (LINK((i, eval(en, a)), en), b)
    let print_value v = print_endline(string_of_int v)
end
