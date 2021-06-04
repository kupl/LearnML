type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff_check (lst : aexp list) (count : int) : int =
  match lst with
  | [] -> count
  | hd :: tl -> (
      match hd with
      | Const num -> diff_check tl count
      | _ -> diff_check tl (count + 1) )


let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const num -> Const 0
  | Var str -> if str = x then Const 1 else Const 0
  | Power (str, num) ->
      if str = x then
        if num <= 1 then Const num
        else Times [ Const num; Power (str, num - 1) ]
      else Const 0
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> (
          let cnt : int = diff_check lst 0 in

          match hd with
          | Const 1 -> diff (Times tl, x)
          | Const __s66 -> Times [ hd; diff (Times tl, x) ]
          | _ ->
              Sum
                [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ]
          ) )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> (
          match hd with _ -> Sum [ diff (hd, x); diff (Sum tl, x) ] ) )
