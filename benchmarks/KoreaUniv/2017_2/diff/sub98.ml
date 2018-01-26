(*problem 4*)


  type aexp = 

  | Const of int

  | Var of string

  | Power of string * int

  | Times of aexp list

  | Sum of aexp list

 

  let rec diff : aexp * string -> aexp

  = fun (e, x) -> 

   match e with

   | Const a -> Const 0;

   | Var v -> if x = v then Const 1 else Const 0

   | Power (b, n) -> if x = b then Times [Const n; Power(b, (n-1))]
                                    else Const 0

   | Times l -> (match l with 
                  | [] -> Const 0
                  | hd::tl -> Sum[(Times ((diff (hd, x))::tl)); Times [hd;(diff ((Times tl),x))]])

   | Sum m-> match m with 
                | [] -> Const 0
                | hd::tl -> Sum [(diff (hd, x)); (diff ((Sum tl), x))]