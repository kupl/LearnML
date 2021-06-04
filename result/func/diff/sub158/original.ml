type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match exp with
  | Const n -> Const 0
  | Var str -> if str = x then Const 1 else Const 0
  | Power (str, n) ->
      if str = x then
        if n = 2 then Times [ Const n; Var x ]
        else Times [ Const n; Power (str, n - 1) ]
      else Const 0
  | Times [ Const n; Var str ] -> if str = x then Const n else Const 0
  | Times [ Const n1; Power (str, n2) ] ->
      if str = x then
        if n2 = 2 then Times [ Const (n1 * n2); Var x ]
        else Times [ Const (n1 * n2); Power (x, n2 - 1) ]
      else Const 0
  | Times lst -> Times lst
  | Sum [ aexp ] -> diff (aexp, x)
  | Sum lst -> (
      match lst with
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
      | [] -> Sum [] )


;;
diff (Sum [ Power ("x", 2); Times [ Const 3; Var "x" ]; Const 4 ], "x")

;;
diff
  ( Sum
      [
        Times [ Const 4; Power ("x", 3) ];
        Power ("x", 2);
        Times [ Const 2; Var "x" ];
        Const 4;
      ],
    "x" )
