type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
    Const _ -> Const 0
    |Var s -> if s=x then Const 1 else Const 0
    |Power (s,a) -> if (s=x && a>0) then Times [Const a;Power (s,a-1)] else Const 0 
    |Times l -> begin
      match l with
        []->Const 1
        |h::t-> begin
        if(List.length t = 1)
          then Sum [Times [diff(h,x); (List.hd t)]; Times [h ; diff((List.hd t),x)]]
          else Sum [Times [diff(h,x); Times t]; Times [h; diff(Times t,x)]]
        end
      end
    |Sum l -> begin
      match l with
        []->Const 0
        |h::t->  Sum [diff (h,x); diff((Sum t),x)]
      end;;
      
      
    
