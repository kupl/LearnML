type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let rec cal ((form : aexp), (var : string)) : aexp =
  match form with
  | Const n -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Power (x, n) ->
      if x = var then
        if n = 0 then Const 0
        else if n = 1 then Const 1
        else Times [ Const n; Power (x, n - 1) ]
      else Const 0
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | [ __s83 ] -> cal (__s83, var)
      | __s84 :: __s85 -> (
          match form with
          | Const 0 -> Const 0
          | _ ->
              Sum
                [
                  Times [ __s84; cal (Times __s85, var) ];
                  Times [ cal (__s84, var); Times __s85 ];
                ] ) )
  | Sum lst -> (
      match lst with
      | [] -> raise InvalidArgument
      | [ l ] -> cal (l, var)
      | l :: r -> Sum [ cal (l, var); cal (Sum r, var) ] )


let notone (x : aexp) : bool = if x = Const 1 then false else true

let notzero (x : aexp) : bool = if x = Const 0 then false else true

let rec arrange (form : aexp) : aexp =
  match form with
  | Times lst ->
      if List.filter notone lst = [] then Const 1
      else Times (Const 1 :: List.filter notone lst)
  | Sum lst ->
      if List.filter notzero lst = [] then Const 0
      else Sum (Const 0 :: List.filter notzero lst)
  | _ -> form


let diff ((form : aexp), (var : string)) : aexp = arrange (cal (form, var))
