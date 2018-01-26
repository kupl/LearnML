(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  (*테스트용 데이터*)
  let t1 = Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1]
  let t2 = Const 14
  let t3 = Var "x"
  let t4 = Power("x", 3)
  let t5 = Times [Power("x", 3); Const 4]
  let t6 = Times [Power("x", 3); Const 3; Const 5]
  let t7 = Times [Const 10]
  let t8 = Sum[Const 18; Const 18; Const 18; Const 18]

  let tt = Times [Const 2; Power("x", 3);]
  let ttt = [Const 2; Power("x", 3);]


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
end






(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int


(* 테스트용 데이터들 위어꺼 두개는 되는거, 밑에두개는 안되는거*)
let t1 = ( SimpleBranch(1,1) , SimpleBranch(1,1) )
let t2 = (CompoundBranch (3,(CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))),SimpleBranch (1, 4))),SimpleBranch (6, 3))
let t3 = ( SimpleBranch(1,10) , SimpleBranch(1,3) )
let t4 = (CompoundBranch (4,(CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))),SimpleBranch (1, 4))),SimpleBranch (6, 3))




(*mobile처리*)
let rec calMob : mobile -> int -> int
= fun mob sumLen ->
  
  match mob with
  |(left, right) -> ( calBranch (left) (-1) sumLen ) + ( calBranch (right) (1) sumLen )


(*branch처리*)
and calBranch : branch -> int -> int -> int
= fun bran direction leng->

  match bran with
  |SimpleBranch(len, wei) -> ( leng + (direction*len) ) * wei
  |CompoundBranch(len, mob) -> calMob mob ( leng + (direction*len) )  




(*왼쪽은 마이너스 오른쪽은 플러스로 하여 토크의 합이 0이 되면 ture인데, 이 함수는 토크의 합 체크용*)
let check : mobile -> int
= fun mob ->  (* TODO *)

  calMob mob 0



let balanced : mobile -> bool
= fun mob ->  (* TODO *)

  (* 입력받은 mobile을 매개변수로 하여 calMob호출. 토크의 합이 0이면 ture*)
  if( (calMob mob 0) = 0 ) then true
  else false


end






(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  (*테스트 데이터*)
  let t1 = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))
  let t2 = ADD(INT 1, INT 3)
  let t3 = SIGMA(INT 1, INT 1, X)
  let t4 = SIGMA(INT 1, INT 2, X)
  let t5 = SIGMA(INT 1, INT 10, X)


  (*count는 SIGMA의 X때문에 쓰임 SIGMA빼고는 걍 아무값이나 없어도 된다*)
  let rec cal : exp -> int -> int
  = fun exp count ->

    match exp with
    |X -> count
    |INT(integer) -> integer 
    |ADD(e1, e2) -> (cal e1 count) + (cal e2 count)
    |SUB(e1, e2) -> (cal e1 count) - (cal e2 count)
    |MUL(e1, e2) -> (cal e1 count) * (cal e2 count)
    |DIV(e1, e2) -> (cal e1 count) / (cal e2 count)
    |SIGMA(start, last, expression) -> sigma (cal start 0) (cal last 0) expression count 


  and sigma : int -> int-> exp -> int -> int
  = fun count last expression sum->
    if( (count-1)=last ) then sum(*루프 끝*)
    else sigma (count+1) last  expression (sum + (cal expression count))   


  let calculator : exp -> int
  = fun exp -> (* TODO *)
    cal exp 0

end





(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

(*테스트데이터*)
let t1 = P("a", V "a")
let t2 = P ("a", P ("a", V "a"))
let t3 = P ("a", P ("b", C (V "a", V "b")))
let t4 = P ("a", C (V "a", P ("b", V "a")))

let t5 = P ("a", V "b")
let t6 = P ("a", C (V "a", P ("b", V "c")))
let t7 = P ("a", P ("b", C (V "a", V "c")))
let t8 = P("a", C( V "c",P("c",V "a")))

let rec ck : exp*string list -> int
= fun (exp, lst)->
  

  match exp with
  |P(va, ex) -> ck(ex, va::lst)
  |C(ex1, ex2) -> ck(ex1, lst) + ck(ex2, lst)
  |V(va) -> if (List.mem va lst = true) then 0 else 1 




(*=====================================================*)
(*스타또!*)
(*=====================================================*)
  let check : exp -> bool
  = fun exp -> (* TODO *)
    
    if(ck(exp, [])=0) then true
    else false

end

