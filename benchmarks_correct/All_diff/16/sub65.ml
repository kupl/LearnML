
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

(*=====================================================*)
(*하나짜리 처리*)
(*=====================================================*)
let rec diffMono : aexp * string -> aexp
= fun (exp, var) ->
    
    match exp with
    |Const( con ) -> Const 0
    |Var( str ) -> if(str=var) then Const 1 else Const 0 (*문자열 한개 미분해야 되는거면 미분해서 1되고, 아니면 상수취급해서 0*)
    |Power( str, integer ) -> 
      if(str=var) then 
          if(integer=1) then 
            Const integer
          else if(integer = 2) then
            Times[ (Const integer); Var str ]
          else
            Times[ (Const integer); Power(str, (integer-1)) ]
      else
        Const 0



(*=====================================================*)
(*Times는 곱의 미분법으로 처리*)
(*=====================================================*)
and dT : aexp * string -> aexp
= fun (exp,var) ->
      
    match exp with
    |Times ( head::tail ) -> 
      if(List.length tail  = 0) then 
        dT(head, var)
      else
        Sum[Times(dT(head, var)::tail); Times[head; dT(Times(tail), var)] ]

    |Sum ( head::tail ) -> 
      if(List.length tail  = 0) then
        dT(head, var)
      else 
        Sum[ dT(head, var); dT(Sum(tail), var) ]
    
    |_ -> diffMono (exp, var)

 
 

(*=====================================================*)
(*시작!*)
(*=====================================================*)
let diff : aexp * string -> aexp
= fun (exp, var) -> (* TODO *)
  match exp with
    |Times ( mlist ) -> dT(exp, var)
    |Sum ( mlist ) -> dT(exp, var)
    |_ -> diffMono (exp, var)