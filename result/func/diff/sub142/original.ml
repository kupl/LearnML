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
  | Var str -> if str = x then Const 1 else Var str
  | Power (str, num) ->
      if str = x then
        if num <= 1 then Const num
        else Times [ Const num; Power (str, num - 1) ]
      else Power (str, num)
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> (
          let cnt : int = diff_check lst 0 in
          if cnt = 0 then Const 0
          else
            match hd with
            | Const num -> Times [ Const num; diff (Times tl, x) ]
            | _ -> (
                match tl with
                | [] -> Times [ diff (hd, x) ]
                | _ -> Times [ diff (hd, x); diff (Times tl, x) ] ) ) )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> (
          match hd with _ -> Sum [ diff (hd, x); diff (Sum tl, x) ] ) )
