type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec isvarpow : aexp -> bool =
 fun aexp -> match aexp with Var t -> true | Power (x, t) -> true | _ -> false


let rec istherevarpow : aexp list -> bool =
 fun l ->
  match l with
  | [] -> false
  | hd :: tl -> if isvarpow hd then true else istherevarpow tl


let aexptolist : aexp -> aexp list =
 fun aexp ->
  match aexp with
  | Times hd :: tl -> hd :: tl
  | Sum hd :: tl -> hd :: tl
  | _ -> []


let rec diff : aexp * string -> aexp =
 fun (aexp, x) ->
  match aexp with
  | Const a -> Const 0
  | Var t -> if t = x then Const 1 else Const 0
  | Power (t, n) ->
      if t = x then Times [ Const n; Power (t, n - 1) ] else Const 0
  | Times hd :: tl ->
      Sum
        ( Times (diff (hd, x) :: tl)
        :: aexptolist (Times (hd :: aexptolist (diff (Times tl, x)))) )
  | Sum hd :: tl -> Sum (diff (hd, x) :: aexptolist (diff (Sum tl, x)))
