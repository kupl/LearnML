type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun func lst ->
  match lst with
  | [] -> []
  | hd::tl -> (func hd)::(map func tl)

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
  | Const n -> Const 0
  | Var y -> if (x <> y) then Const 0 else Const 1
  | Power (y, n) -> 
    if (x <> y) then Const 0 
    else if (n < 0) then raise (Failure "Invalid Input")
    else if n = 0 then Const 0
    else Times [Const n; Power (y, n-1)]
  | Times es -> 
    begin match es with
    | [] -> raise (Failure "Invalid Input")
    | [e] -> diff (e, x)
    | hd::tl -> Sum [Times ((diff (hd, x))::tl); Times [hd; diff (Times tl, x)]]
    end
  | Sum es -> Sum (map (fun e -> diff (e, x)) es) 
;;
