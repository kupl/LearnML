type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((a : aexp), (s : string)) : aexp =
  match a with
  | Const i -> Const 0
  | Var v -> if v = s then Const 1 else Const 0
  | Power (v, p) ->
      if v = s then Times [ Const p; Power (v, p - 1) ] else Const 0
  | Times __s61 :: __s62 ->
      Sum
        [
          Times [ __s61; diff (Times __s62, s) ];
          Times (diff (__s61, s) :: __s62);
        ]
  | Times l ->
      Sum
        (List.map
           (fun (x : aexp) ->
             Times (diff (x, s) :: List.filter (fun (y : aexp) -> y != x) l))
           l)
  | Sum l -> Sum (List.map (fun (x : aexp) -> diff (x, s)) l)
