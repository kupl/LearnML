type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const i -> Const 0
  | Var a -> if a = "x" then Const 1 else Const 0
  | Power (a, i) -> Times [ Const i; Power (a, i - 1) ]
  | Times h :: t -> Times [ h; diff (Sum t, x) ]
  | Sum h :: t -> Sum [ diff (h, x); diff (Sum t, x) ]
  | _ -> Const 0
prlui-iMac:engine prl$ subl temp.ml
prlui-iMac:engine prl$ ocamlformat temp.ml
type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const i -> Const 0
  | Var a -> if a != x then Const 0 else Const 1
  | Power (a, i) ->
      if a != x then Const 0 else Times [ Const i; Power (a, i - 1) ]
  | Sum h :: t -> Sum [ diff (h, x); diff (Sum t, x) ]
  | Times __s62 -> (
      match __s62 with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ] )
  | _ -> Const 0