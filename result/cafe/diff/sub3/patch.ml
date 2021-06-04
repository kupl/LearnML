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
      if v != s || p = 0 then Const 0
      else if p = 1 then Const 1
      else Times [ Const p; Power (v, p - 1) ]
  | Times l -> (
      match l with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, s) :: __s64);
              Times [ __s63; diff (Times __s64, s) ];
            ] )
  | Sum l ->
      Sum
        (List.filter
           (fun (x : aexp) -> x != Const 0)
           (List.map (fun (x : aexp) -> diff (x, s)) l))
