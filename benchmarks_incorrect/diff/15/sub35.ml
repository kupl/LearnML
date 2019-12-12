type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff(aexp,y)=
  match aexp with
  | Const a -> Const 0
  | Var x-> if x=y then Const 1 else Const 0
  | Power(x,a) -> if x=y then
                    if a = 0 then Const 0
                    else if a=1 then Const 1
                    else Times [Const a; Power(x,a-1)]
                  else Const 0
  | Times l ->if List.mem (Const 0) l then Const 0
              else (match l with
                  | hd::[] ->diff(aexp,y)
                  | hd::tl ->match hd with
                            | Const a -> Times[hd;diff(Times tl,y)]
                            | _ ->Sum [Times [diff(hd,y);Times tl]; Times[hd;diff(Times tl,y)]])
  | Sum l ->(match l with
             | hd::[] -> diff(hd,y)
             | hd::tl -> Sum [diff(hd,y);diff(Sum tl,y)]);;


