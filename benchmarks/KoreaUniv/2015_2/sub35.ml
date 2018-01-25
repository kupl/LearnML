
(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
 if pred(hd) then hd::pred(tl) else pred(tl);;


(*********************)
(* Problem 2: zipper *)
(*********************)

let rec zipper : int list * int list -> int list
=fun (a,b) -> [] (* TODO *)
let rec zipper(a,b)=
  match a with
  |[] -> []
  |hd1::tl1->
    match b with
    |[]->[]
    |hd2::tl2::-> if hd1>hd2 then hd2::zipper(a,tl2) else hd1::zipper(tl1,b) ;; 


  

(*******************)
(* Problem 3: iter *)
(*******************)


let rec iter(n,f)=
if n = 0 then (fun x -> x) else fun x->f(iter(n-1,f) x) ;;


(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun(aexp,x) -> aexp
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
                  | hd::[] ->diff(aexp,y);
                  | hd::tl ->match hd with
                            | Const a -> Times[hd;diff(Times tl,y)]
                            | _ ->Sum [Times [diff(hd,y);Times tl]; Times[hd;diff(Times tl,y)]])
  | Sum l ->(match l with
             | hd::[] -> diff(hd,y)
             | hd::tl -> Sum [diff(hd,y);diff(Sum tl,y)]);;






(*************************)
(* Problem 5: Calculator *)
(*************************)type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let calculator : exp -> int
=fun e -> 0
(*
let calculator exp =
  match exp with
  |INT a -> a
  |ADD (exp1,exp2) -> match exp1 with
                      | INT a ->match exp2 with
                                  | INT b -> subcal2(a,b)
                                  | X ->SIG(temp_exp1,temp_exp2,ADD(a,X)) 
                      | X ->match exp2 with
                            | INT b ->SIG(temp_exp1,temp_exp2,ADD(X,b))
                            | X ->SIG(temp_exp1, temp_exp2, ADD(X,X)) 
  |SUB (exp1,exp2) -> match exp1 with
                      | INT a ->match exp2 with
                                  | INT b -> a - b
                                  | X ->SIG(temp_exp1,temp_exp2,SUB(a,X)) 
                      | X ->match exp2 with
                            | INT b ->SIG(temp_exp1,temp_exp2,SUB(X,b))
                            | X ->SIG(temp_exp1, temp_exp2, SUB(X,X)) 
  |MUL (exp1,exp2) -> match exp1 with
                      | INT a ->match exp2 with
                                | INT b -> a * b
                                | X ->SIG(temp_exp1,temp_exp2,MUL(a,X))
                      | X ->match exp2 with
                            | INT b -> SIG(temp_exp1,temp_exp2,MUL(X,b))
                            | X -> SIG(temp_exp1,temp_exp2,MUL(X,X))           
  |DIV (exp1,exp2) -> match exp1 with
                      | INT a ->match exp2 with
                                | INT 0 -> Raise(failure "This equation is impposible to calculate")
                                | INT b ->a / b
                                | X ->SIG(temp_exp1,temp_exp2,DIV(a,X))
                      | X ->match exp2 with
                            | INT 0 -> Raise(failure "This equation is impposible to calculate")
                            | INT b -> SIG(temp_exp1,temp_exp2,DIV(X,b))
                            | X -> SIG(temp_exp1,temp_exp2,INT 1)                      
  |SIG (exp1,exp2,exp3) ->if exp1 = X then  Raise(failure "This equation is impposible to calculate")
                          else if exp2 = X then  Raise(failure "This equation is impposible to calculate")
                          else if exp1<exp2 then  Raise(failure "This equation is impposible to calculate")
                          else sigma(exp1,exp2,exp3)
                        *)
let rec subcal1 : exp*exp->int
  = fun (n, exp) ->
    match exp with
    | X -> subcal1(n,n)
    | INT(a) -> a
    | ADD(x, y) -> subcal1(n,x) + subcal1(n,y)
    | SUB(x, y) -> subcal1(n,x) - subcal1(n,y)
    | MUL(x, y) -> subcal1(n,x) * subcal1(n,y)
    | DIV(x, y) -> subcal1(n,x) / subcal1(n,y)

let rec sigma : exp*exp*exp-> int
  = fun(a,b,f)->
    if subcal1(a,a) > subcal1(b,b) then subcal1(a,f)+sigma(INT(subcal1(a,a)),b,f)
    else subcal1(a,f)

let rec subcal2 exp =
  match exp with
  |INT(a) -> a
  |ADD(x,y) -> subcal2(x)+subcal2(y)
  |SUB(x,y) -> subcal2(x)-subcal2(y)
  |MUL(x,y) -> subcal2(x)*subcal2(y)
  |DIV(x,y) -> subcal2(x)/subcal2(y)
  |SIGMA(a,b,f) ->match f with
                  |SIGMA(x,y,g)->(subcal2(b)-subcal2(a)+1)*sigma(x,y,g)
                  |_->sigma(a,b,f)

let calculator e = subcal2(e);;  