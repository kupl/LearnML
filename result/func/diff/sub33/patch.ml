type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec exist ((l : aexp list), (x : string)) : int =
  match l with
  | [] -> 0
  | hd :: tl -> (
      match hd with
      | Var v -> if v = x then 1 + exist (tl, x) else exist (tl, x)
      | Power (str, i) -> if str = x then 1 + exist (tl, x) else exist (tl, x)
      | Times lst -> exist (lst, x) + exist (tl, x)
      | Sum lst -> exist (lst, x) + exist (tl, x)
      | _ -> 0 + exist (tl, x) )


let rec modify ((l : aexp list), (x : string)) : aexp list =
  match l with
  | [] -> []
  | hd :: tl -> (
      match hd with
      | Const n -> Times [ Power (x, 0); Const n ] :: modify (tl, x)
      | _ -> hd :: modify (tl, x) )


let normalized ((l : aexp), (x : string)) : aexp =
  let modified : aexp list = modify ([ l ], x) in

  match modified with [] -> raise Failure "error" | hd :: tl -> hd


let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const __s58 -> Const 0
  | Var __s59 -> if __s59 = x then Const 1 else Const 0
  | Power (__s60, __s61) ->
      if __s60 = x then
        match __s61 with
        | 0 -> Const 0
        | 1 -> Const 1
        | 2 -> Times [ Const 2; Var x ]
        | _ -> Times [ Const __s61; Power (__s60, __s61 - 1) ]
      else Const 0
  | Times __s62 -> (
      match __s62 with
      | [] -> Const 1
      | [ __s63 ] -> diff (__s63, x)
      | __s64 :: __s65 -> (
          match __s64 with
          | Const 1 -> diff (Times __s65, x)
          | Const __s66 -> Times [ Const __s66; diff (Times __s65, x) ]
          | _ ->
              Sum
                [
                  Times (diff (__s64, x) :: __s65);
                  Times [ __s64; diff (Times __s65, x) ];
                ] ) )
  | Sum __s67 -> (
      match __s67 with
      | [] -> Const 0
      | [ __s68 ] -> diff (__s68, x)
      | __s69 :: __s70 -> Sum [ diff (__s69, x); diff (Sum __s70, x) ] )
