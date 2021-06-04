type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec consume_times (l : aexp list) : aexp list =
  let rec inner (l : aexp list) (const : int) : aexp list =
    match l with
    | Const n :: tl -> inner tl (n * const)
    | hd :: tl -> hd :: inner tl const
    | [] -> [ Const const ]
  in
  inner l 1


let rec diff ((exp : aexp), (x : string)) : aexp =
  let rec diff_sum (l : aexp list) : aexp list =
    match l with hd :: tl -> diff (hd, x) :: diff_sum tl | [] -> []
  in

  let rec diff_times (l : aexp list) : aexp list =
    match l with
    | Times t :: tl -> diff_times (t @ tl)
    | Const n :: tl -> Const n :: diff_times tl
    | Power (v, n) :: tl ->
        if x = v then
          if n = 1 then diff_times tl
          else if n = 2 then Const n :: Var v :: diff_times tl
          else Const n :: Power (v, n - 1) :: diff_times tl
        else Power (v, n) :: diff_times tl
    | Var v :: tl ->
        if x = v then Const 1 :: diff_times tl else Var v :: diff_times tl
    | hd :: tl -> hd :: diff_times tl
    | [] -> []
  in

  match exp with
  | Const _ -> Const 0
  | Var v -> if x = v then Const 1 else Const 0
  | Power (v, num) ->
      if x = v then
        if num = 1 then Const 1
        else if num = 2 then Times [ Const num; Var v ]
        else Times [ Const num; Power (v, num - 1) ]
      else Const 0
  | Times l -> (
      match l with
      | [ __s63 ] -> diff (__s63, x)
      | __s64 :: __s65 ->
          Sum
            [
              Times (diff (__s64, x) :: __s65);
              Times [ __s64; diff (Times __s65, x) ];
            ]
      | [] -> Const 0 )
  | Sum l -> Sum (diff_sum l)
