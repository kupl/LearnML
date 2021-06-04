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


let rec __s58 ((__s59 : aexp), (__s60 : string)) : aexp =
  match __s59 with
  | Const __s63 -> Const 0
  | Var __s64 -> if __s64 = __s60 then Const 1 else Const 0
  | Power (__s65, __s66) ->
      if __s65 = __s60 then Times [ Const __s66; Power (__s65, __s66 - 1) ]
      else Const 0
  | Times __s67 :: __s68 ->
      Sum
        [
          Times [ __s58 (__s67, __s60); Times __s68 ];
          Times [ __s67; __s58 (Times __s68, __s60) ];
        ]
  | Times [] -> Const 0
  | Sum __s69 :: __s70 -> Sum [ __s58 (__s69, __s60); __s58 (Sum __s70, __s60) ]
  | Sum [] -> Const 0


let rec __s61 (__s62 : aexp) : aexp = match __s62 with _ -> __s62

let diff ((e : aexp), (x : string)) : aexp = __s61 (__s58 (e, x))
