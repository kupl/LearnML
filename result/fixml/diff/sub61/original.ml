type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (aexp, x) ->
  let rec diffList : aexp list -> aexp list =
   fun al ->
    match al with
    | [] -> []
    | [ e ] -> [ diff (e, x) ]
    | hd :: tl -> diff (hd, x) :: diffList tl
  in

  match aexp with
  | Const c -> Const 0
  | Var v -> Var v
  | Power (v, c) ->
      if v = x then Times [ Const c; Power (v, c - 1) ] else Const 0
  | Times l -> (
      match l with [] -> Times [] | hd :: tl -> Times (diffList l) )
  | Sum l -> ( match l with [] -> Sum [] | hd :: tl -> Sum (diffList l) )
