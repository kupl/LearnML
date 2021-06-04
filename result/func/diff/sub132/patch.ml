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
  | Const __s75 -> Const 0
  | Sum __s76 -> (
      match __s76 with
      | [] -> Sum []
      | [ __s77 ] -> __s58 (__s77, __s60)
      | __s78 :: __s79 -> (
          match __s78 with
          | Const _ -> __s58 (Sum __s79, __s60)
          | _ ->
              Sum
                (List.append
                   [ __s58 (__s78, __s60) ]
                   [ __s58 (Sum __s79, __s60) ]) ) )
  | Power (__s80, __s81) ->
      if __s60 = __s80 then
        if __s81 = 1 then Const 1
        else Times [ Const __s81; Power (__s80, __s81 - 1) ]
      else Const 0
  | Times __s82 -> (
      match __s82 with
      | [] -> Const 0
      | [ __s83 ] -> __s58 (__s83, __s60)
      | __s84 :: __s85 -> (
          match __s84 with
          | Const 0 -> Const 0
          | _ ->
              Sum
                [
                  Times [ __s84; __s58 (Times __s85, __s60) ];
                  Times [ __s58 (__s84, __s60); Times __s85 ];
                ] ) )
  | Var __s86 -> if __s60 = __s86 then Const 1 else Const 0


let rec __s61 (__s62 : aexp) : aexp =
  match __s62 with
  | Const __s63 -> Const __s63
  | Var __s64 -> Var __s64
  | Power (__s65, __s66) -> Power (__s65, __s66)
  | Times __s67 -> (
      match __s67 with
      | [] -> Const 0
      | [ __s68 ] -> __s61 __s68
      | __s69 :: __s70 -> (
          match __s69 with
          | Times [] -> Times [ __s61 (Times __s70) ]
          | Sum [] -> Times [ __s61 (Times __s70) ]
          | Const 1 -> Times [ __s61 (Times __s70) ]
          | Const 0 -> Const 0
          | _ -> Times (List.append [ __s69 ] [ __s61 (Times __s70) ]) ) )
  | Sum __s71 -> (
      match __s71 with
      | [] -> Const 0
      | [ __s72 ] -> __s61 __s72
      | __s73 :: __s74 -> (
          match __s73 with
          | Times [] -> Sum [ __s61 (Times __s74) ]
          | Sum [] -> Sum [ __s61 (Times __s74) ]
          | Const 0 -> Sum [ __s61 (Times __s74) ]
          | _ -> Sum (List.append [ __s73 ] [ __s61 (Times __s74) ]) ) )


let diff ((e : aexp), (x : string)) : aexp = __s61 (__s58 (e, x))
