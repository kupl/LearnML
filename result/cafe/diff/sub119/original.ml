(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp =
 fun (e, x) ->
  (* TODO *)
  let rec helper (e, x) =
    match e with
    | Const a -> Const 0
    | Var v -> if v = x then Const 1 else Const 0
    | Power (s, i) ->
        if s = x then Times [ Const i; Power (s, i - 1) ] else Const 0
    (*|Times lst -> begin
        match lst with
        |h::t -> Sum ([Times [helper (h, x)] @t] @ [Times [h; helper (t, x)]])
        end
      |Sum lst2 -> begin
        match lst2 with
        |h::t -> Sum (helper (h, x)::helper (Sum t, x))
        end*)
  in
  helper (e, x)
