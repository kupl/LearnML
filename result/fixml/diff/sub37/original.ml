type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff (aexp, str) =
  match aexp with
  | Const n -> Const 0
  | Var x -> if x = str then Const 1 else Const 0
  | Power (x, n) ->
      if n = 1 then Const 1
      else if x = str then Times [ Const n; Power (x, n - 1) ]
      else Const 0
  | Times alist -> (
      match alist with
      | [] -> raise Failure "Empty List"
      | [ hd ] -> diff (hd, str)
      | hd :: tl -> (
          match hd with
          | Const 0 -> Const 0
          | Const 1 -> diff (Times tl, str)
          | Const n -> Times [ hd; diff (Times tl, str) ]
          | Var x -> Times [ diff (hd, str); diff (Times tl, str) ]
          | Power (x, n) ->
              Times [ diff (Power (x, n), str); diff (Times tl, str) ]
          | Times alist2 ->
              Sum
                [
                  Times [ diff (Times alist2, str); Times tl ];
                  Times [ Times alist2; diff (Times tl, str) ];
                ]
          | Sum alist2 ->
              Sum
                [
                  Times [ diff (Sum alist2, str); Times tl ];
                  Times [ Sum alist2; diff (Times tl, str) ];
                ] ) )
  | Sum alist -> (
      match alist with
      | [] -> raise Failure "Empty List"
      | [ hd ] -> diff (hd, str)
      | hd :: tl -> (
          match hd with
          | Const 0 -> Const 0
          | Const n -> diff (Sum tl, str)
          | Var x -> Sum [ diff (Var x, str); diff (Sum tl, str) ]
          | Power (x, n) -> Sum [ diff (Power (x, n), str); diff (Sum tl, str) ]
          | Times alist2 -> Sum [ diff (Times alist2, str); diff (Sum tl, str) ]
          | Sum alist2 -> Sum [ diff (Sum alist2, str); diff (Sum tl, str) ] ) )
