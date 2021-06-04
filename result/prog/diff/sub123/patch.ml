type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec __s4 ((__s5 : aexp), (__s6 : string)) : aexp =
  match __s5 with
  | Sum __s64 -> (
      match __s64 with
      | [] -> Const 0
      | [ __s65 ] -> __s4 (__s65, __s6)
      | __s66 :: __s67 -> Sum [ __s4 (__s66, __s6); __s4 (Sum __s67, __s6) ] )
  | Times __s68 -> (
      match __s68 with
      | [] -> Const 1
      | [ __s69 ] -> __s4 (__s69, __s6)
      | __s70 :: __s71 -> (
          match __s70 with
          | Const __s72 -> Times [ Const __s72; __s4 (Times __s71, __s6) ]
          | _ ->
              Sum
                [
                  Times [ __s70; __s4 (Times __s71, __s6) ];
                  Times (__s4 (__s70, __s6) :: __s71);
                ] ) )
  | Power (__s73, __s74) ->
      if __s73 = __s6 then
        match __s74 with
        | 0 -> Const 0
        | 1 -> Const 1
        | _ -> Times [ Const __s74; Power (__s73, __s74 - 1) ]
      else Const 0
  | Const __s75 -> Const 0
  | Var __s76 ->
      if __s76 = __s6 then Sum [ Const 1; Times [ Var __s76; Const 0 ] ]
      else Const 0


let diff ((e : aexp), (x : string)) : aexp =
  let rec diff_list ((e : aexp), (x : string)) : aexp list =
    match e with
    | Sum [ Const a ] -> [ Const 0 ]
    | Sum (Var x) :: tl -> [ Const 1 ]
    | Sum (Times [ Const a; Var x ]) :: tl -> [ Const a ]
    | Sum (Power (x, 1)) :: tl -> [ Const 1 ]
    | Sum (Times [ Const a; Power (x, 1) ]) :: tl -> [ Const a ]
    | Sum (Power (x, 2)) :: tl ->
        Times [ Const 2; Var x ] :: diff_list (Sum tl, x)
    | Sum (Times [ Const a; Power (x, 2) ]) :: tl ->
        Times [ Const (a * 2); Var x ] :: diff_list (Sum tl, x)
    | Sum (Power (x, n)) :: tl ->
        Times [ Const n; Power (x, n - 1) ] :: diff_list (Sum tl, x)
    | Sum (Times [ Const a; Power (x, n) ]) :: tl ->
        Times [ Const (a * n); Power (x, n - 1) ] :: diff_list (Sum tl, x)
    | _ -> raise Failure "Error"
  in
  __s4 (e, x)
