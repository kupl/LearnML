type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec isvarpow (aexp : aexp) : bool =
  match aexp with Var t -> true | Power (x, t) -> true | _ -> false


let rec istherevarpow (l : aexp list) : bool =
  match l with
  | [] -> false
  | hd :: tl -> if isvarpow hd then true else istherevarpow tl


let aexptolist (aexp : aexp) : aexp list =
  match aexp with
  | Times hd :: tl -> hd :: tl
  | Sum hd :: tl -> hd :: tl
  | _ -> []


let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const a -> Const 0
  | Var t -> if t = x then Const 1 else Const 0
  | Power (t, n) ->
      if t = x then Times [ Const n; Power (t, n - 1) ] else Const 0
  | [ Times __s62 ] -> diff (__s62, x)
  | Times hd :: tl ->
      Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ]
  | Sum hd :: tl -> Sum (diff (hd, x) :: aexptolist (diff (Sum tl, x)))
  | Sum __s65 -> Sum (List.map (fun (__s66 : aexp) -> diff (__s66, x)) __s65)
