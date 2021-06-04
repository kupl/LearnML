type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff_times ((lst : aexp list), (x : string), (b : int)) : aexp =
  match lst with
  | [] -> Const 0
  | hd :: tl -> (
      match hd with
      | Const a -> diff_times (tl, x, a + b)
      | Var a -> if a = x then Const b else diff_times (tl, x, b)
      | Power (a, k) ->
          if a = x then
            if k > 1 then Times [ Const (b * k); Power (a, k - 1) ]
            else Const (b * k)
          else Const 0
      | _ -> diff_times (tl, x, b) )


let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Sum lst ->
      Sum
        ( match lst with
        | [] -> []
        | hd :: tl -> [ diff (hd, x); diff (Sum tl, x) ] )
  | Times lst -> diff_times (lst, x, 0)
  | Var a -> if a = x then Const 1 else Const 0
  | Const a -> Const 0
  | Power (a, b) ->
      if b > 1 then Times [ Const b; Power (a, b - 1) ] else Const b
