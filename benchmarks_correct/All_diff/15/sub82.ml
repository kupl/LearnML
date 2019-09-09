type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec iscontainconstzero 
=fun lst -> 
  match lst with
    [] -> false
   |hd::tl -> if hd = Const 0 then true else iscontainconstzero tl

exception InvalidArgument
let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
  match aexp with 
    Const n -> Const 0
   |Var v -> 
       if v = x then Const 1 else Const 0 
   |Power (p, n) ->
       if p = x then
          (match n with
               0 -> Const 0
              |1 -> Const 1
              |_ -> Times [Const n; Power(p, n-1)]
          )
       else Const 0
   |Times t ->
       if iscontainconstzero t then Const 0
       else 
            (match t with 
              [] -> raise InvalidArgument
             |hd::[] -> diff (hd, x)
             |hd::tl -> 
                 (match hd with
                     Const 1 -> diff(Times tl, x)
                    |Const n -> Times [hd; diff(Times tl, x)]
                    |_ -> Sum [Times (diff(hd, x)::tl); Times [hd; diff(Times tl, x)]] 
                 )
            )

   |Sum s ->
       match s with
           [] -> raise InvalidArgument
          |hd::[] -> diff (hd, x)
          |hd::tl -> Sum[ diff(hd, x); diff(Sum tl, x); ]
