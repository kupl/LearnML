type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec calcPower : aexp * string -> aexp =
 fun (power, var) ->
  match (power, var) with
  | Power (variable, exponent), diffVar ->
      if variable = diffVar then
        Times [ Const exponent; Power (variable, exponent - 1) ]
      else Power (variable, exponent)
  | _, _ -> raise Failure "wrong input for Power Calculation"


let rec contains : aexp list * string -> bool =
 fun (l, var) ->
  match (l, var) with
  | hd :: tl, diffVar ->
      if hd = Var diffVar then true
      else if tl = [] then false
      else contains (tl, diffVar)
  | _, _ -> raise Failure "wrong input for contains check"


let rec noZero : aexp list -> bool =
 fun l ->
  match l with
  | hd :: tl ->
      if hd = Const 0 then false else if tl = [] then true else noZero tl
  | _ -> raise Failure "wrong input for zero check"


let rec calcTimes : aexp list * string -> aexp list =
 fun (l, var) ->
  match (l, var) with
  | [], diffVar -> []
  | Const i :: tl, diffVar -> Const i :: calcTimes (tl, diffVar)
  | Var s :: tl, diffVar ->
      if s = diffVar then calcTimes (tl, diffVar)
      else Var s :: calcTimes (tl, diffVar)
  | Power (x, exponent) :: tl, diffVar ->
      calcPower (Power (x, exponent), diffVar) :: calcTimes (tl, diffVar)
  | _, _ -> raise Failure "wrong input for multiplication"


let rec calcSum : aexp list * string -> aexp list =
 fun (l, var) ->
  match (l, var) with
  | [], diffVar -> []
  | Const i :: tl, diffVar ->
      if tl = [] then [ Const 0 ] else calcSum (tl, diffVar)
  | Var s :: tl, diffVar ->
      if s = diffVar then [ Const 1 ] else calcSum (tl, diffVar)
  | Power (variable, exponent) :: tl, diffVar ->
      if variable = diffVar then
        calcPower (Power (variable, exponent), diffVar) :: calcSum (tl, diffVar)
      else calcSum (tl, diffVar)
  | Times x :: tl, diffVar ->
      if contains (x, diffVar) && noZero l then
        calcTimes (x, diffVar) @ calcSum (tl, diffVar)
      else calcSum (tl, diffVar)
  | _, _ -> raise Failure "wrong input for sum"


let diff : aexp * string -> aexp =
 fun (exp, var) ->
  match (exp, var) with
  | Const c, diffVar -> Const 0
  | Var s, diffVar -> if s = diffVar then Const 1 else Const 0
  | Power (variable, exponent), diffVar ->
      if variable = diffVar then calcPower (Power (variable, exponent), diffVar)
      else Const 0
  | Times l, diffVar ->
      if contains (l, diffVar) && noZero l then Times (calcTimes (l, diffVar))
      else Const 0
  | Sum l, diffVar -> Sum (calcSum (l, diffVar))
