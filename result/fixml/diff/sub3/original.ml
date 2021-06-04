type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((a : aexp), (s : string)) =
  match a with
  | Const i -> Const 0
  | Var v -> if v = s then Const 1 else Const 0
  | Power (v, p) ->
      if v != s || p = 0 then Const 0
      else if p = 1 then Const 1
      else Times [ Const p; Power (v, p - 1) ]
  | Times l ->
      Sum
        (List.map
           (fun x ->
             if diff (x, s) = Const 0 then Const 0
             else if diff (x, s) = Const 1 then
               Times (List.filter (fun y -> y != x) l)
             else Times (diff (x, s) :: List.filter (fun y -> y != x) l))
           l)
  | Sum l ->
      Sum
        (List.filter
           (fun x -> x != Const 0)
           (List.map (fun x -> diff (x, s)) l))
