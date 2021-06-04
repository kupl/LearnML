type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp =
 fun (exp, var) ->
  let rec f e v =
    match e with
    | Const a -> Const 0
    | Var a -> if a = var then Const 1 else Const 0
    | Power (x, y) ->
        if x = var then Times [ Const y; Power (x, y - 1) ] else Const 0
    | Sum lst -> (
        match lst with
        | [] -> raise Failure " No elements in the list! "
        | hd :: tl ->
            if tl = [] then f hd var else Sum ([ f hd var ] @ [ f (Sum tl) var ])
        )
    | Times lst -> (
        match lst with
        | [] -> raise Failure " No elements in the list! "
        | hd :: tl -> (
            if tl = [] then f hd var
            else
              match hd with
              | Const a -> Times ([ hd ] @ [ f (Times tl) var ])
              | Var a ->
                  if hd = Var var then
                    Sum ([ f (Times tl) var ] @ [ hd ] @ [ f (Times tl) var ])
                  else Times ([ hd ] @ [ f (Times tl) var ])
              | Power (x, y) ->
                  if Var x = Var var then
                    Sum
                      ( [ Times ([ f hd var ] @ tl) ]
                      @ [ Times ([ hd ] @ [ f (Times tl) var ]) ] )
                  else Times ([ hd ] @ [ f (Times tl) var ])
              | Sum ist ->
                  Sum
                    ( [ Times ([ f hd var ] @ tl) ]
                    @ [ Times ([ hd ] @ [ f (Times tl) var ]) ] )
              | Times ist ->
                  Sum
                    ( [ Times ([ f hd var ] @ tl) ]
                    @ [ Times ([ hd ] @ [ f (Times tl) var ]) ] ) ) )
  in
  f exp var
