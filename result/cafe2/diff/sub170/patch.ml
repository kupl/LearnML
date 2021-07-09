type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let listToelement2 (lst : aexp list) : aexp =
  match lst with
  | hd :: tl -> (
      match hd with
      | Times l -> Times l
      | Sum l -> Sum l
      | Const x -> Const x
      | Var x -> Var x
      | Power (s, n) -> Power (s, n) )


let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const n -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power (s, n) ->
      if s = x then Times [ Const n; Power (s, n - 1) ] else Const 0
  | Times [] -> Const 0
  | Times __s62 :: __s63 ->
      Sum
        [
          Times (diff (__s62, x) :: __s63);
          Times [ __s62; diff (Times __s63, x) ];
        ]
  | Times l -> diff_Times l x
  | Sum [] -> Const 0
  | Sum __s64 :: __s65 -> Sum [ diff (__s64, x); diff (Sum __s65, x) ]
  | Sum l -> diff_Sum l x


and diff_Times (l : aexp list) (x : string) : aexp =
  match l with
  | hd :: tl ->
      Sum
        [
          Times [ diff (hd, x); listToelement2 tl ];
          Times [ hd; diff (listToelement2 tl, x) ];
        ]


and diff_Sum (l : aexp list) (x : string) : aexp =
  match l with hd :: tl -> Sum [ diff (hd, x); diff (listToelement2 tl, x) ]


let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")