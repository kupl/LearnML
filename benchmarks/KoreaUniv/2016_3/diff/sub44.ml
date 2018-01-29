
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
= fun (aex, str) -> match aex with
    | Const n -> Const 0
    | Var a -> if a = str then Const 1 else Var a
    | Power (a,b) -> if a = str then Times[Const b; Power (a, b-1)] else Power (a,b)
    | Times a -> tImes (a, str)
    | Sum a -> sUm (a, str)
and tImes : aexp list * string -> aexp
= fun (lst, str) -> match lst with
    | [] -> Const 1
    | [h] -> diff (h, str)
    | hd::tl -> Sum([Times([diff (hd,str)]@tl)]@[Times ([hd]@[tImes (tl, str)])])
and sUm : aexp list * string -> aexp
= fun (lst, str) -> match lst with
    | [] -> Const 0
    | hd::tl -> Sum ([diff (hd,str)]@[sUm (tl, str)])