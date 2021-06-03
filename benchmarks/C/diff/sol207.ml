type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
    | Const n -> Const 0
    | Var s -> if s = x then Const 1 else Const 0
    | Power (s , n) -> if s = x then Times[Const n; Power(s,n-1)] else Const 0
    | Times e -> ( match e with 
                    | [] -> Const 0
                    | hd::tl -> Sum [Times(diff (hd, x) :: tl) ; Times[hd;diff(Times tl,x)] ]
                 )
    | Sum e->(match e with
                | []-> Const 0
                | hd::tl -> Sum [ diff(hd,x) ; diff(Sum tl,x) ]
              );;
              
diff( Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1] , "x");;
diff(Times[Const 3; Power("x",2);Var "y"] ,"x");;