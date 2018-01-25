(*2014210080 Choi Kyuhyeon*)


(*Problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror t = 
  match t with
  |Empty -> Empty
  |Node(p, lc, rc) -> Node(p, mirror rc, mirror lc)



(*Problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd nat1 nat2 = 
  match nat1 with
  |ZERO -> nat2
  |SUCC x -> natadd x (SUCC nat2)

let rec natmul nat1 nat2 = 
  match nat1 with
  |ZERO -> ZERO
  |SUCC ZERO -> nat2
  |SUCC x -> natadd nat2 (natmul x nat2)

let rec natexp nat1 nat2 = 
  match nat2 with
  |ZERO -> SUCC ZERO
  |SUCC x -> natmul nat1 (natexp nat1 x)



(*Problem 3*)
type formula = 
   True
  |False
  |Var of string
  |Neg of formula
  |And of formula * formula
  |Or of formula * formula
  |Imply of formula * formula
  |IFF of formula * formula 

let e = []

let rec extend x l = 
  match l with
  |[] -> x::l
  |hd::tl -> if x=hd then l 
             else hd::(extend x tl)

let rec cntelt l =
  match l with
  |[] -> 0
  |hd::tl -> 1 + (cntelt tl)

let rec cnt f e =
  match f with
  |True -> 0
  |False -> 0
  |Var x -> if (cntelt (extend x e))=1 then 0 else 1 
  |Neg f1 -> cnt f1 e
  |And (f1, f2) -> (cnt f1 e) + (cnt f2 e)
  |Or (f1, f2) -> (cnt f1 e) + (cnt f2 e)
  |Imply (f1, f2) -> (cnt f1 e) + (cnt f2 e)
  |IFF (f1, f2) -> (cnt f1 e) + (cnt f2 e)

let rec evalt f = 
  match f with
  |True -> true
  |False -> false
  |Var x -> true
  |Neg f1 -> if (evalt f1)=true then false else true
  |And (f1, f2) -> (evalt f1)&&(evalt f2)
  |Or (f1, f2) -> (evalt f1)||(evalt f2)
  |Imply (f1, f2) -> if ((evalt f1)=true)&&((evalt f2)=false) then false else true  
  |IFF (f1, f2) -> if (evalt f1)=(evalt f2) then true else false

let rec evalf f = 
  match f with
  |True -> true
  |False -> false
  |Var x -> false
  |Neg f1 -> if (evalf f1)=true then false else true
  |And (f1, f2) -> (evalf f1)&&(evalf f2)
  |Or (f1, f2) -> (evalf f1)||(evalf f2)
  |Imply (f1, f2) -> if ((evalf f1)=true)&&((evalf f2)=false) then false else true
  |IFF (f1, f2) -> if (evalf f1)=(evalf f2) then true else false

let sat f =
  if (cnt f e)>0 then true
  else (evalt f)||(evalf f)



(*Problem 4*)
type aexp = 
  |Const of int
  |Var of string
  |Power of string * int
  |Times of aexp list
  |Sum of aexp list

let rec diff (a, x) =
  match a with
  |Const n -> Const 0
  |Var y -> if y<>x then Const 0 else Const 1
  |Power (y, n) -> if y<>x then Const 0 else Times [Const n; Power (y, n-1)]
  |Times l -> 
    (match l with
    |[] -> Const 0
    |hd::tl -> Sum [Times (diff(hd, x)::tl); Times [hd; diff(Times tl, x)]])
  |Sum l -> 
    (match l with
    |[] -> Const 0
    |hd::tl -> Sum [diff (hd ,x); diff (Sum tl, x)])



(*Problem 5*)
type exp = X
  |INT of int
  |ADD of exp * exp
  |SUB of exp * exp
  |MUL of exp * exp
  |DIV of exp * exp
  |SIGMA of exp * exp * exp

let empty_env = []

let rec extend_env a b env =
  match a<=b with
  |false -> env
  |true -> a::(extend_env (a+1) b env)

let get_hd env = 
  match env with
  |[] -> raise (Failure "there is no value in env")
  |hd::tl -> hd

let rec fold f x l = 
  match l with
  |[] -> 0
  |hd::tl -> (f x [hd]) + (fold f x tl)

let rec eval exp env =
  match exp with
  |X -> get_hd env
  |INT n -> n 
  |ADD (e1, e2) -> 
    let n1 = eval e1 env in
    let n2 = eval e2 env in
      n1 + n2
  |SUB (e1, e2) ->
    let n1 = eval e1 env in
    let n2 = eval e2 env in
      n1 - n2
  |MUL (e1, e2) ->
    let n1 = eval e1 env in                                                  
    let n2 = eval e2 env in
      n1 * n2
  |DIV (e1, e2) ->
    let n1 = eval e1 env in                                                    
    let n2 = eval e2 env in
      n1 / n2
  |SIGMA (e1, e2, e3) ->
    let n1 = eval e1 env in
    let n2 = eval e2 env in
    fold (fun x env -> eval x env) e3 (extend_env n1 n2 env)

let calculator exp = eval exp empty_env



(*Problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec fw: mobile -> int 
= fun mb ->
  match mb with
  |(SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
  |(SimpleBranch (l1, w1), CompoundBranch (l2, mb2)) -> w1 + (fw mb2)
  |(CompoundBranch (l1, mb1), SimpleBranch (l2, w2)) -> (fw mb1) + w2
  |(CompoundBranch (l1, mb1), CompoundBranch (l2, mb2)) -> (fw mb1) + (fw mb2)

let rec balanced: mobile -> bool 
= fun mb ->
  match mb with
  |(SimpleBranch (l1, w1), SimpleBranch (l2, w2)) 
    -> if l1*w1 = l2*w2 then true else false
  |(SimpleBranch (l1, w1), CompoundBranch (l2, mb2)) 
    -> if l1*w1 = l2*(fw mb2) then true else false
  |(CompoundBranch (l1, mb1), SimpleBranch (l2, w2)) 
    -> if l1*(fw mb1)=  l2*w2 then true else false
  |(CompoundBranch (l1, mb1), CompoundBranch (l2, mb2)) 
    -> if l1*(fw mb1) = l2*(fw mb2) then true else false



(*Problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec rev dl = 
  match dl with
  |[] -> []
  |hd::tl -> (rev tl) @ [hd]

let rec todec dl =
  match rev dl with
  |[] -> raise (Failure "error")
  |[ZERO] -> 0
  |[ONE] -> 1
  |hd::tl -> (todec [hd]) + 2*(todec (rev tl))

let rec tobin n = 
  match n with
  |0 -> [ZERO]
  |1 -> [ONE]
  |_ -> (tobin (n/2))@(tobin (n mod 2))  

let rec bmul dl1 dl2 = tobin ((todec dl1) * (todec dl2))































  
