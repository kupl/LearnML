type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec reverse (lst : 'b list) : 'a list =
  match lst with [] -> lst | hd :: tl -> reverse tl @ [ hd ]


let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (a, b) ->
      if a = x then
        match b with
        | 2 -> Times [ Const 2; Var a ]
        | _ -> Times [ Const b; Power (a, b - 1) ]
      else Const 0
  | Times l -> (
      match l with
      | [] -> Const 0
      | __s69 :: __s70 ->
          Sum
            [
              Times (diff (__s69, x) :: __s70);
              Times [ __s69; diff (Times __s70, x) ];
            ]
      | _ -> (
          let newList : aexp list = reverse l in

          match newList with
          | [] -> Times l
          | hd :: tl -> Times (diff (hd, x) :: tl) ) )
  | Sum l -> (
      match l with
      | [] -> Sum l
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
