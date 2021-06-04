type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec __s1 (__s2 : aexp) : aexp =
  match __s2 with
  | Power (_, 0) -> Const 1
  | Times __s73 ->
      if List.mem (Const 0) __s73 then Const 0
      else if List.length __s73 = 1 then __s1 (List.nth __s73 1)
      else Times __s73
  | Sum __s74 -> Sum (List.map __s1 __s74)
  | _ -> __s2


let rec diff ((a : aexp), (s : string)) : aexp =
  match a with
  | Const i -> Const 0
  | Var v -> if v = s then Const 1 else Const 0
  | Power (v, p) ->
      if v != s || p = 0 then Const 0
      else if p = 1 then Const 1
      else Times [ Const p; Power (v, p - 1) ]
  | Times l -> (
      match l with
      | [] -> invalid_arg "invalid value!"
      | [ __s65 ] ->
          __s1 (Sum (List.map (fun (__s72 : aexp) -> diff (__s72, s)) l))
      | __s66 :: __s67 ->
          __s1
            (Sum
               [
                 Times [ diff (__s66, s); Times __s67 ];
                 Times [ __s66; diff (Times __s67, s) ];
               ]) )
  | Sum l ->
      Sum
        (List.filter
           (fun (x : aexp) -> x != Const 0)
           (List.map (fun (x : aexp) -> diff (x, s)) l))
