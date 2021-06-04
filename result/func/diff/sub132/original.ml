type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec exists_var ((f : aexp), (x : string)) : bool =
  match f with
  | Sum [] -> false
  | Times [] -> false
  | Sum hd :: tl ->
      if exists_var (hd, x) || exists_var (Sum tl, x) then true else false
  | Var a -> if a = x then true else false
  | Power (a, n) -> if a = x then true else false
  | Times hd :: tl ->
      if exists_var (hd, x) || exists_var (Times tl, x) then true else false
  | Const n -> false


let rec find_element ((f : aexp), (x : string)) : aexp =
  match f with
  | Sum [] -> Const 0
  | Times [] -> Const 0
  | Sum hd :: tl ->
      if exists_var (hd, x) then
        Sum [ find_element (hd, x); find_element (Sum tl, x) ]
      else find_element (Sum tl, x)
  | Var a -> if a = x then Var a else Const 0
  | Power (a, n) -> if a = x then Power (x, n) else Const 0
  | Times lst -> if exists_var (Times lst, x) then Times lst else Const 0
  | Const _ -> Const 0


let rec differ ((f : aexp), (x : string)) : aexp =
  match f with
  | Sum [] -> Const 0
  | Times [] -> Const 1
  | Var a -> if a = x then Const 1 else Var a
  | Power (a, n) ->
      if a = x then
        if n = 1 then Const 1 else Times [ Const n; Power (a, n - 1) ]
      else Power (a, n)
  | Times hd :: tl -> Times [ differ (hd, x); differ (Times tl, x) ]
  | Sum hd :: tl -> Sum [ differ (hd, x); differ (Sum tl, x) ]
  | Const a -> Const a


let diff ((e : aexp), (x : string)) : aexp = differ (find_element (e, x), x)
