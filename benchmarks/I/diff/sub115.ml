(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> let rec f (e, x) = match e with
                                  | Const n -> Const 0
                                  | Var n -> if n = x then Const 1 else Const 0
                                  | Power (n, i) -> if n = x then Times [Const i; Power (n, i-1)] else Const 0
                                  | Times lst -> begin match lst with
                                                 |[] -> Const 0
                                                 |h::t -> begin match h with
                                                          | Var n -> if n = x then Const 1 else Times [h; f(Times t, x)]
                                                          | Power (n, i) -> if n = x then Times ([Const i; Power(n, i-1)]@t) else Times [h; f(Times t, x)]
                                                          | _ -> Times [h; f(Times t, x)] end 
                                                 end
                                  | Sum lst -> begin match lst with
                                               |[] -> Const 0
                                               |h::t -> Sum [f (h, x); f(Sum t, x)] end in 
              f (e, x)
