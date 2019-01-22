type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff(sic,v) =
  match sic with
    | Const n -> Const 0
    | Var x -> if x=v then Const 1 else Const 0
    | Power(x,n) -> if x=v then
              if n=0 then Const 0
              else if n=1 then Const 1
              else Times [Const n;Power(x,n-1)] 
            else Const 0
    | Times l ->if List.mem (Const 0) l then Const 0
          else (match l with
            | h::[] -> diff(h,v)
            | h::t -> match h with
                  | Const n -> Times[h;diff(Times t,v)]
                  | _ -> Sum [Times [diff(h,v);Times t];Times [h;diff(Times t,v)]])
    | Sum l ->(match l with
          | h::[] -> diff(h,v)
          | h::t -> Sum [diff(h,v);diff(Sum t,v)]);;
