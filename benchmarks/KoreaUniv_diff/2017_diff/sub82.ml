(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e, x) -> (* TODO *)
    match e with
    | Const a -> Const 0
    | Var a-> if (a=x) then Const 1 else Const 0
    | Power (a, b) -> 
        if (a=x) then Times [Const b ; Power (a, b-1)]
        else Const 0  
    | Times a -> (
        match a with
        [] -> Const 0
        | hd::tl -> (
            Sum [ Times([diff (hd, x)] @ tl) ; Times [hd ; diff (Times tl, x)]]
            )
        )
    | Sum  a -> (
        match a with
        [] -> Const 0
        | hd::tl -> Sum [diff(hd, x) ; diff ((Sum tl), x)]
        )
;;
