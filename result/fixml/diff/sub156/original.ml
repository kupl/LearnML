type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec reverse : 'a list -> 'a list =
 fun lst -> match lst with [] -> lst | hd :: tl -> reverse tl @ [ hd ]


let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match exp with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Var a
  | Power (a, b) ->
      if a = x then
        match b with
        | 2 -> Times [ Const 2; Var a ]
        | _ -> Times [ Const b; Power (a, b - 1) ]
      else Const 0
  | Times l -> (
      match l with
      | [] -> Times l
      | _ -> (
          let newList = reverse l in

          match newList with
          | [] -> Times l
          | hd :: tl -> Times (diff (hd, x) :: tl) ) )
  | Sum l -> (
      match l with
      | [] -> Sum l
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
