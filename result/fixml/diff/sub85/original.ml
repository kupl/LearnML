type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec contain : aexp list * string -> bool =
 fun (exp, var) ->
  match (exp, var) with
  | [], x -> false
  | Const a :: tl, x -> contain (tl, x)
  | Var a :: tl, x -> if a = x then true else contain (tl, x)
  | Power (x, a) :: tl, y -> if x = y && a != 0 then true else contain (tl, x)
  | _, _ -> raise Failure "Invalid input"


let rec timesHelper : aexp list * string -> aexp list =
 fun (exp, var) ->
  match (exp, var) with
  | [], x -> []
  | Const a :: tl, x -> Const a :: timesHelper (tl, x)
  | Var a :: tl, x ->
      if a = x then Const 1 :: timesHelper (tl, x)
      else Var a :: timesHelper (tl, x)
  | Power (x, a) :: tl, y ->
      if x = y && a != 0 then
        Times [ Const a; Power (x, a - 1) ] :: timesHelper (tl, y)
      else Power (x, a) :: timesHelper (tl, y)
  | _, _ -> raise Failure "Invalid input"


let rec sumHelper : aexp list * string -> aexp list =
 fun (exp, var) ->
  match (exp, var) with
  | [], x -> []
  | Const a :: tl, x -> Const 0 :: sumHelper (tl, x)
  | Var a :: tl, x ->
      if a = x then Const 1 :: sumHelper (tl, x) else sumHelper (tl, x)
  | Power (x, a) :: tl, y ->
      if x = y && a != 0 then
        Times [ Const a; Power (x, a - 1) ] :: sumHelper (tl, y)
      else Const 0 :: sumHelper (tl, y)
  | Times t :: tl, x ->
      if contain (t, x) then Times (timesHelper (t, x)) :: sumHelper (tl, x)
      else Const 0 :: sumHelper (tl, x)
  | _, _ -> raise Failure "Invalid input"


let rec diff : aexp * string -> aexp =
 fun (exp, var) ->
  match (exp, var) with
  | Const a, x -> Const 0
  | Var a, x -> if a = x then Const 1 else Const 0
  | Power (x, a), y ->
      if x = y && a != 0 then Times [ Const a; Power (x, a - 1) ] else Const 0
  | Times t, x -> if contain (t, x) then Times (timesHelper (t, x)) else Const 0
  | Sum s, x -> Sum (sumHelper (s, x))
