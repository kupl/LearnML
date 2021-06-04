type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let rec diff ((aexp : aexp), (str : string)) : aexp =
  match aexp with
  | Const n -> Const 0
  | Var s -> if s = str then Const 1 else Const 0
  | Power (s, n) ->
      if s = str then Times [ Const n; Power (s, n - 1) ] else Const 0
  | Sum [] -> raise InvalidArgument
  | Sum el -> Sum (List.map (fun (x : aexp) -> diff (x, str)) el)
  | Times el -> (
      match el with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, str) :: __s64);
              Times [ __s63; diff (Times __s64, str) ];
            ] )
