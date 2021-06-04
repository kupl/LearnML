type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec __s1 (__s2 : aexp list) : bool =
  match __s2 with
  | [] -> false
  | __s73 :: __s74 -> if __s73 = Const 0 then true else __s1 __s74


let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const a -> Const 0
  | Var b -> if b = x then Const 1 else Const 0
  | Power (b, a) ->
      if b = x then Times [ Const a; Power (x, a - 1) ] else Const 0
  | Times l -> (
      if __s1 l then Const 0
      else
        match l with
        | [] -> raise Failure "Exception(Template)"
        | [ __s65 ] -> diff (__s65, x)
        | __s66 :: __s67 -> (
            match aexp with
            | Const 1 -> diff (Times __s67, x)
            | Const __s68 -> Times [ __s66; diff (Times __s67, x) ]
            | _ ->
                Sum
                  [
                    Times (diff (__s66, x) :: __s67);
                    Times [ __s66; diff (Times __s67, x) ];
                  ] ) )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
