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
  let rec f ((e : aexp), (x : string)) : aexp =
    match e with
    | Const n -> Const 0
    | Var n -> if n = x then Const 1 else Const 0
    | Power (n, i) ->
        if n = x then Times [ Const i; Power (n, i - 1) ] else Const 0
    | Times lst -> (
        match lst with
        | [] -> Const 0
        | h :: t -> (
            match h with
            | Var n -> if n = x then Const 1 else Times [ h; f (Times t, x) ]
            | Power (n, i) ->
                if n = x then Times ([ Const i; Power (n, i - 1) ] @ t)
                else Times [ h; f (Times t, x) ]
            | _ -> Times [ h; f (Times t, x) ] ) )
    | Sum lst -> (
        match lst with
        | [] -> Const 0
        | h :: t -> Sum [ f (h, x); f (Sum t, x) ] )
  in
  __s4 (e, x)
