(* problem 4 *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
let extract 
= fun l -> match l with
  | [] -> []
  | hd::tl -> hd
let rec diff : aexp * string -> aexp
= fun (e, x) -> match e with
  | Const(a) -> Const 0
  | Var(s) -> if (s = x) then Const 1 else Const 0
  | Power(s, i) -> if (s = x) then Times [Const (i); Power(s, (i-1))] else Const 0
  | Times(l) -> begin match l with
          | [] -> Const 0
          | hd::tl -> Sum [Times(diff(hd, x)::tl); Times[hd; diff(Times(tl), x)]]
          end
  | Sum(l) -> begin match l with
          | [] -> Const 0
          | hd::tl -> Sum [(diff (hd, x)); (diff (Times(tl), x))]
          end
      