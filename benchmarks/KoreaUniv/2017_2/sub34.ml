(*problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
match t with
 Empty -> Empty
|Node(i,x,y)-> Node(i,(mirror y),(mirror x))


(* problem 2*)
type nat = ZERO | SUCC of nat
let rec length n = 
  match n with
  ZERO -> 0
  |SUCC(x) -> 1+ (length x)

let rec mksuc n =
  match n with
  0-> ZERO
  |x-> SUCC(mksuc (x-1))


let natadd : nat -> nat -> nat 
= fun n1 n2 ->
  let sum = (length n1) + (length n2) in
    mksuc sum;;


let natmul : nat -> nat -> nat 
= fun n1 n2 -> 
  let mul = (length n1) * (length n2) in
    mksuc mul;;

let natexp : nat -> nat -> nat 
= fun n1 n2 -> 
  let exp = (float (length n1))**(float (length n2)) in
    mksuc (int_of_float exp);;




(*problem 3*)
let rec fastexpt : int -> int -> int
= fun b n ->
if n = 0 then 1 else
(if n mod 2 = 0 
  then fastexpt b (n/2) * fastexpt b (n/2) 
else fastexpt b (n-1) * b);;


type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

type value3 = Bool of bool
type env3 = (string * value3) list
  let empty_env3 = []
  let extend_env3 (x,v) e = (x,v)::e
  let rec apply_env3 e x =
  match e with
  [] -> raise (Failure ("variable" ^x^"not found"))
  | (y,v)::tl -> if x = y then v else apply_env3 tl x

let rec eval3: formula -> env3 -> value3
= fun formula env3 ->
  match formula with
   True -> Bool true
  |False -> Bool false
  |Var x -> (apply_env3 env3 x)
  |Neg e ->
    if (eval3 e env3)= Bool false then Bool true
    else Bool false
  |And (e1,e2)->
  if (eval3 e1 env3)= Bool true then
    if (eval3 e2 env3) = Bool true then Bool true
    else Bool false
  else Bool false
  |Or (e1,e2) ->
  if (eval3 e1 env3)= Bool false then
    if (eval3 e2 env3) = Bool false then Bool false
    else Bool true
  else Bool true
  |Imply (e1,e2) ->
    if (eval3 e1 env3) = Bool true then
      if (eval3 e2 env3) = Bool false then Bool true
      else Bool false
    else Bool false
  
  |Iff (e1,e2) ->
    if (eval3 e1 env3) = (eval3 e2 env3) then Bool true
    else Bool false

let vs = []
let rec findv formula = 
  match formula with
   Var x -> [x]
  |Neg x-> (findv x)@vs
  |And(x,y)->(findv y)@((findv x)@vs)
  |Or(x,y)->(findv y)@((findv x)@vs)
  |Iff(x,y)->(findv y)@((findv x)@vs)
  |Imply(x,y)-> (findv y)@((findv x)@vs)
  |_ -> []

let rec lstleng lst=
  match lst with
  []->0
  |hd::tl -> 1+(lstleng tl)



let rec binary num=
  match num with
  0 -> []
  |_ -> (binary (num/2))@[(num mod 2)]

let rec signexten lst fulllst=
  if lstleng lst < lstleng fulllst then signexten ([0]@lst) fulllst
  else lst

let rec set numlst lst=   
  match numlst with
  []-> []
  |head::tail->
  if head =0 then
    match lst with
    []-> []
    |hd::tl-> (hd,Bool false)::(set tail tl)
  else
      match lst with
    []-> []
    |hd::tl-> (hd,Bool true)::(set tail tl)


let rec st formula n=
let v1 = fastexpt 2 (lstleng (findv formula)) in
  if n=v1 then Bool false
  else
    if eval3 formula (set (signexten (binary n) (binary (v1-1))) (findv formula)) = Bool true then
    Bool true
    else st formula (n+1)


let sat : formula -> bool
= fun f -> 
let v1= st f 0 in
  match v1 with
  Bool n -> n
  



 
(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list



let rec chuchul lst x=
match lst with
[]-> []
|hd::tl-> if hd = x then chuchul tl x
else hd::chuchul tl x

let rec diff2
= fun (e,x) count sumcount ls->
  match e with
  Times lst ->
    (match lst with
    []->Times[Const count]
    |hd::tl->
    
      (match hd with
        Const n -> Times[Const n;(diff2 ((Times tl),x) count sumcount (Const n::ls))]
        |Var y->
          if y=x then
            if count=0 then Times[Const 1;(diff2 ((Times tl),x) (count+1) sumcount (Var x::ls))]
            else Times[Var x;(diff2 ((Times tl),x) (count+1) sumcount (Var x::ls))]
           
        else Times[Var y;(diff2 ((Times tl),x) count sumcount (Var y::ls))]
        |Power(y,n)->
          if y=x then
            if count=0 then Times[Power(y,n-1);(diff2 ((Times tl),x) (count+n) sumcount (Power(y,n)::ls))]
          else Times[Power(y,n);(diff2 ((Times tl),x) (count+n) sumcount (Power(y,n)::ls))]
        else Times[Power(y,n);(diff2 ((Times tl),x) count sumcount (Power(y,n)::ls))]
        |Sum lst2-> if count = 0 then Sum[Times[(diff2 (Times (ls@tl),x) count sumcount ((Sum lst2)::ls));(Sum lst2)];Times[Times((Const 1)::(ls@tl));(diff2 ((Sum lst2),x) count sumcount ((Sum lst2)::ls))]]
          else  Sum[Times[(diff2 (Times (ls@tl),x) 0 sumcount ((Sum lst2)::ls));(Sum lst2)];Times[Times((Const 1)::(ls@tl));(diff2 ((Sum lst2),x) count sumcount ((Sum lst2)::ls))]]
        |Times lst2 -> Times[(diff2 ((Times lst2),x) count sumcount ls);(diff2 ((Times tl),x) count sumcount ls)]
      )
    )
  |Sum lst ->
    (match lst with
      []->Sum[Const 0]
      |hd::tl->
        (match hd with
          Const n-> Sum[Const 0;(diff2 ((Sum tl),x) count sumcount ls)]
          |Var y->
            if y=x then
              if sumcount=0 then
                  Sum[Const 1;(diff2 ((Sum tl),x) count 1 ls)]
              else Sum[Const 1;(diff2 ((Sum tl),x) count sumcount ls)]
            else Sum[Const 0;(diff2 ((Sum tl),x) count sumcount ls)]
          |Power(y,n)->
          if y=x then
            if sumcount=0 then

                Sum[Times[Const n;Power(y,n-1)];(diff2 ((Sum tl),x) count 1 ls)]
            else Sum[Times[Const n;Power(y,n-1)];(diff2 ((Sum tl),x) count sumcount ls)]
          else Sum[Const 0;(diff2 ((Sum tl),x) count sumcount ls)]
          |Sum lst2-> Sum[(diff2 ((Sum lst2),x) count sumcount ls);(diff2 ((Sum tl),x) count sumcount ls)]
          |Times lst2-> Sum[(diff2 ((Times lst2),x) count sumcount []);(diff2 ((Sum tl),x) count sumcount ls)]
        )




    )
    
  |Const n-> Const 0
  |Var y-> if y=x then Const 1
    else Const 0
  |Power(y,n)-> if y=x then Times[Const n;Power(y,n-1)]
  else Const 0

let rec diff : aexp * string -> aexp
= fun (e,x) -> diff2 (e,x) 0 0 []





(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

type value5 = Int of int

type env5 = (value5)

let apply_env5 e=
  match e with
  Int n-> Int n

let rec eval5 :exp->env5->value5
= fun exp env ->
  match exp with
  X-> apply_env5 env
  |INT n-> Int n
  |ADD(e1,e2)->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    (match v1,v2 with
    Int n1,Int n2 -> Int(n1+n2))
  |SUB(e1,e2)->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    (match v1, v2 with
    Int n1, Int n2-> Int(n1-n2))
  |MUL (e1,e2) ->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    (match v1, v2 with
    Int n1, Int n2-> Int (n1*n2))
  |DIV (e1,e2)->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    (match v1, v2 with
    Int n1, Int n2-> Int (n1/n2))
  |SIGMA(e1,e2,e3) ->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    let v3 = eval5 e3 v1 in
    if v1 <= v2 then
      match v1,v2,v3 with
      Int n1, Int n2, Int n3 -> eval5 (ADD((INT n3),SIGMA(INT (n1+1),INT n2,e3))) (Int n1)
    else Int 0





let calculator : exp -> int
= fun e ->
let v0 = eval5 e (Int 0) in
match v0 with
Int n -> n






(* problem 6*)
type mobile = branch * branch    (*left and rihgt branches*)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let lft tuple = 
  match tuple with
  (x,y)->x
let rgt tuple =
  match tuple with
  (x,y)-> y

let rec wgt branch =
  match branch with
  SimpleBranch(x,y)-> y
  |CompoundBranch(x,y)-> (wgt (lft y))+(wgt (rgt y))

let toq exp =
  match exp with
  |SimpleBranch(n1,n2)-> n1*n2
  |CompoundBranch(n,branch)-> n*((wgt (lft branch)) + (wgt(rgt branch)))

let rec bal exp =
  match exp with
  (x,y)->
  if toq x = toq y then
  (match x,y with
    CompoundBranch(n1,b1) , CompoundBranch(n2,b2)-> (bal b1)&&(bal b2)
    |CompoundBranch(n1,b1), SimpleBranch(k1,k2) -> bal b1
    |SimpleBranch(k1,k2), CompoundBranch(n1,b1) -> bal b1
    |SimpleBranch(k1,k2), SimpleBranch(k3,k4) -> true)
  else false



let balanced : mobile -> bool
= fun m ->
let v0 = bal m in
match v0 with
true-> true
|false-> false


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec todec b1 =
  let v0 = (lstleng b1)-1 in
  match b1 with
  []->0
  |hd::tl->
  if hd=ONE then (fastexpt 2 v0)+(todec tl)
else 0 + todec tl

let rec tobin num =
  match num with
  0->[]
  |n->
  if n mod 2=0 then (tobin (n/2))@[ZERO]
  else (tobin (n/2))@[ONE]


let bmul : bin -> bin -> bin
= fun b1 b2 -> tobin ((todec b1)*(todec b2))