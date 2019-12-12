type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
  
  
let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
    | Var a -> 
      if a = x then Const 1
      else Const 0
    | Power (a, b) -> 
      if a = x then Times [Const b; Power (a, b - 1) ]
      else Const 0
    | Sum lst ->
      let rec addition : aexp list -> aexp list -> aexp
        = fun l result ->
        match l with
          | [] -> Sum result
          | hd :: tl -> addition tl (result @ [diff (hd, x)])
      in addition lst []
    | Times lst ->
      let copy_lst = lst in
      let rec multiple : int -> aexp list -> aexp list -> aexp
        = fun flag l result ->
          match l with
            | [] -> Sum result
            | hd :: tl ->
              if copy_lst = l
                then if flag = 0 
                  then multiple 1 (tl @ [hd]) (result @ [Times ([diff (hd, x)] @ tl)])
                  else Sum result
              else multiple 1 (tl @ [hd]) (result @ [Times ([diff (hd, x)] @ tl)])
      in multiple 0 lst []
    | Const a -> Const 0 ;;
      