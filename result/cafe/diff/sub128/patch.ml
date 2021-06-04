type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec chuchul (lst : 'a list) x : 'a list =
  match lst with
  | [] -> []
  | hd :: tl -> if hd = x then chuchul tl x else hd :: chuchul tl x


let rec diff2 ((e : aexp), (x : string)) (count : int) (sumcount : int)
    (ls : aexp list) : aexp =
  match e with
  | Times lst -> (
      match lst with
      | [] -> Times [ Const count ]
      | hd :: tl -> (
          match hd with
          | Const n ->
              Times
                [ Const n; diff2 (Times tl, x) count sumcount (Const n :: ls) ]
          | Var y ->
              if y = x then
                if count = 0 then
                  Times
                    [
                      Const 1;
                      diff2 (Times tl, x) (count + 1) sumcount (Var x :: ls);
                    ]
                else
                  Times
                    [
                      Var x;
                      diff2 (Times tl, x) (count + 1) sumcount (Var x :: ls);
                    ]
              else
                Times
                  [ Var y; diff2 (Times tl, x) count sumcount (Var y :: ls) ]
          | Power (y, n) ->
              if y = x then
                if count = 0 then
                  Times
                    [
                      Power (y, n - 1);
                      diff2 (Times tl, x) (count + n) sumcount
                        (Power (y, n) :: ls);
                    ]
                else
                  Times
                    [
                      Power (y, n);
                      diff2 (Times tl, x) (count + n) sumcount
                        (Power (y, n) :: ls);
                    ]
              else
                Times
                  [
                    Power (y, n);
                    diff2 (Times tl, x) count sumcount (Power (y, n) :: ls);
                  ]
          | Sum lst2 ->
              if count = 0 then
                Sum
                  [
                    Times
                      [
                        diff2
                          (Times (ls @ tl), x)
                          count sumcount (Sum lst2 :: ls);
                        Sum lst2;
                      ];
                    Times
                      [
                        Times (Const 1 :: (ls @ tl));
                        diff2 (Sum lst2, x) count sumcount (Sum lst2 :: ls);
                      ];
                  ]
              else
                Sum
                  [
                    Times
                      [
                        diff2 (Times (ls @ tl), x) 0 sumcount (Sum lst2 :: ls);
                        Sum lst2;
                      ];
                    Times
                      [
                        Times (Const 1 :: (ls @ tl));
                        diff2 (Sum lst2, x) count sumcount (Sum lst2 :: ls);
                      ];
                  ]
          | Times lst2 ->
              Times
                [
                  diff2 (Times lst2, x) count sumcount ls;
                  diff2 (Times tl, x) count sumcount ls;
                ] ) )
  | Sum lst -> (
      match lst with
      | [] -> Sum [ Const 0 ]
      | hd :: tl -> (
          match hd with
          | Const n -> Sum [ Const 0; diff2 (Sum tl, x) count sumcount ls ]
          | Var y ->
              if y = x then
                if sumcount = 0 then
                  Sum [ Const 1; diff2 (Sum tl, x) count 1 ls ]
                else Sum [ Const 1; diff2 (Sum tl, x) count sumcount ls ]
              else Sum [ Const 0; diff2 (Sum tl, x) count sumcount ls ]
          | Power (y, n) ->
              if y = x then
                if sumcount = 0 then
                  Sum
                    [
                      Times [ Const n; Power (y, n - 1) ];
                      diff2 (Sum tl, x) count 1 ls;
                    ]
                else
                  Sum
                    [
                      Times [ Const n; Power (y, n - 1) ];
                      diff2 (Sum tl, x) count sumcount ls;
                    ]
              else Sum [ Const 0; diff2 (Sum tl, x) count sumcount ls ]
          | Sum lst2 ->
              Sum
                [
                  diff2 (Sum lst2, x) count sumcount ls;
                  diff2 (Sum tl, x) count sumcount ls;
                ]
          | Times lst2 ->
              Sum
                [
                  diff2 (Times lst2, x) count sumcount [];
                  diff2 (Sum tl, x) count sumcount ls;
                ] ) )
  | Const n -> Const 0
  | Var y -> if y = x then Const 1 else Const 0
  | Power (y, n) ->
      if y = x then Times [ Const n; Power (y, n - 1) ] else Const 0


let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const __s58 -> Const 0
  | Var __s59 -> if __s59 = x then Const 1 else Const 0
  | Power (__s60, __s61) ->
      if __s60 = x then Times [ Const __s61; Power (x, __s61 - 1) ] else Const 0
  | Times [] -> Const 0
  | Times __s62 :: __s63 ->
      Sum
        [
          Times (diff (__s62, x) :: __s63);
          Times [ __s62; diff (Times __s63, x) ];
        ]
  | Sum [] -> Const 0
  | Sum __s64 :: __s65 -> Sum [ diff (__s64, x); diff (Sum __s65, x) ]
