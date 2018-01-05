
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

    type value = int option
    type environment = (id * value) list

    let emptyEnv : environment = []
    let rec eval : environment * expr -> value =
      fun (envr, expr) ->
        let apply : (int -> int -> int) -> expr -> expr -> value =
          fun oper e1 e2 ->
            let v1 = eval (envr, e1)
            and v2 = eval (envr, e2)
            in match (v1, v2) with
            | (_, None) -> None
            | (None, _) -> None
            | (Some n1, Some n2) -> Some (oper n1 n2)
        in match expr with
        | NUM n -> Some n
        | PLUS (e1, e2) ->
            apply ( + ) e1 e2
        | MINUS (e1, e2) ->
            apply ( - ) e1 e2
        | MULT (e1, e2) ->
            apply ( * ) e1 e2
        | DIVIDE (e1, e2) ->
            apply ( / ) e1 e2
        | MAX l ->
            let rec findMax : expr list -> value -> value =
              fun l max ->
                match (l, max) with
                | ([], None) -> Some 0
                | ([], Some m) -> max
                | (h::t, None) ->
                    findMax t (eval (envr, h))
                | (h::t, Some m) ->
                    let curr = eval (envr, h)
                    in match curr with
                    | None -> None
                    | Some n ->
                        if n > m then findMax t curr
                        else findMax t max
            in (match l with
            | [] -> Some 0
            | _ -> findMax l None)
        | VAR id ->
            (try List.assoc id envr with _ -> None)
        | LET (id, e1, e2) ->
            let new_envr = (id, (eval (envr, e1)))::(List.remove_assoc id envr)
            in eval (new_envr, e2)

    let print_value : value -> unit =
      fun v ->
        match v with
        | None -> raise (Error "FreeVariable")
        | Some n -> print_int n ; print_string "\n"
  end

