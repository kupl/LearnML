(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec f t = match t with
                          | Node(n,a,b) ->  if a = Empty && b = Empty then Node(n,b,a) else Node(n,f b,f a)
                          | Empty -> Empty
  in f t 


(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> let rec f n = match n with
                            | ZERO -> 0
                            | SUCC(b) -> f b + 1
 in let rec f2 n = if n = 0 then ZERO else SUCC(f2 (n-1))
  in f2 ((f n1)+(f n2))

let natmul : nat -> nat -> nat
= fun n1 n2 -> let rec f n = match n with
                            | ZERO -> 0
                            | SUCC(b) -> f b + 1
              in let rec f2 n = if n = 0 then ZERO else SUCC(f2 (n-1))
  in f2 ((f n1)*(f n2))


let natexp : nat -> nat -> nat
= fun n1 n2 -> let rec f n = match n with
                            | ZERO -> 0
                            | SUCC(b) -> f b + 1
              in let rec f2 n = if n = 0 then ZERO else SUCC(f2 (n-1))
              in let rec f3 a b = if b = 0 then 1 else a*f3 a (b-1)
  in f2 ( f3 (f n1) (f n2))

  
(* problem 3*)
type formula = 
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f ->
           let rec lst_env e lst = (match lst with
                                    | hd::tl -> if tl = [] then[e::hd] else (e::hd)::lst_env e tl
                                | []->[e]::[]
               )
           in let rec case_env e = (match e with
                                | hd::tl -> (lst_env (hd,true) (case_env tl)) @ (lst_env (hd,false) (case_env tl))
                                | [] -> []
               )
           in let rec apply_env x e =
           ( match e with
            | [] -> raise (Failure ("variable " ^ x ^ " not found"))
            | (y,v)::tl -> if x = y then v else apply_env x tl
           )
            in let rec mk_env exp = (match exp with
                            | True -> []
                            | False -> []
                            | Var x -> [x]
                            | Neg e -> mk_env e
                            | And(e1,e2) -> ((mk_env e1)@(mk_env e2))
                            | Or(e1,e2) -> (mk_env e1)@(mk_env  e2)
                            | Imply(e1,e2) -> (mk_env e1)@(mk_env e2)
                            | Iff(e1,e2) -> (mk_env e1)@(mk_env e2)
                )
            in let rec eval exp env = (match exp with
                            | True -> true
                            | False -> false
                            | Var x -> apply_env x env
                            | Neg(e) -> (match eval e env with
                                      | true -> false
                                      | false -> true)
                            | And(e1,e2) -> let v1 = eval e1 env 
                                            in let v2 = eval e2 env 
                                            in v1 && v2
                            | Or(e1,e2) -> let v1 = eval e1 env 
                                           in let v2 = eval e2 env 
                                           in v1 || v2
                            | Imply(e1,e2) -> let v1 = eval e1 env
                                              in let v2 = eval e2 env 
                                              in if v1 then v2 else true
                            | Iff(e1,e2) -> let v1 = eval e1 env 
                                            in let v2 = eval e2 env 
                                            in if v1=v2 then true else false
                            )
          in let rec chk_envlst func elst = ( match elst with 
                            |hd::tl ->
                                if tl = [] then eval func hd 
                                else
                                if (chk_envlst func tl) then true else (eval func hd)
                          
                            |[] -> eval f []
                            )
         in chk_envlst f (case_env (mk_env f))


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> 
              let rec rev_list lst num = (match lst with
                                      | hd::tl -> if List.length tl = num then []@rev_list tl num
                                      else [hd]@rev_list tl num
                                      | [] -> []
                  )
              in let rec tl_list e = (match e with
                                    | hd::tl -> Sum(tl)
                                    |[] -> Sum([])
                  )
              in let rec differ e x =( match e with
                                  | Const n -> Const 0
                                  | Var a -> if a=x then Const 1
                                             else Var a
                                  | Power (a,b) -> if a=x then Times[Const (b);Power(a,(b-1))]
                                                    else Const 0
                                  | Times(lst) -> let rec times_diff lst2 = (match lst2 with
                                                                          | hd::tl -> if tl= [] then Times(differ hd x::rev_list lst (List.length tl))
                                                                          else Sum(Times(differ hd x::rev_list lst (List.length tl))::[times_diff tl])
                                                                          | []-> Sum([Const 0])
                                    )     in times_diff lst
                                  | Sum(lst) -> (match lst with 
                                                |hd::tl->
                                                   if tl=[] then Sum([differ hd x])
                                                   else Sum(differ hd x ::[differ (tl_list lst) x])
                                                |[] -> Sum[Const 0]
                                  )  
                  )
  in differ e x



(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e ->
          let rec eval exp env =  match exp with
                             | X -> env
                             | INT (n) ->  n
                             | ADD (a1, b1) -> let v1 = eval a1 env in let v2 = eval b1 env in v1 + v2
                             | SUB (a1, b1) -> let v1 = eval a1 env in let v2 = eval b1 env in v1 - v2
                             | MUL (a1, b1) -> let v1 = eval a1 env in let v2 = eval b1 env in v1 * v2
                             | DIV (a1, b1) -> let v1 = eval a1 env in let v2 = eval b1 env in v1 / v2
                             | SIGMA(a1,a2,b1) -> let v1 = eval a1 env in let v2 = eval a2 env in let rec func a = if a = v2 then  eval b1 a else eval b1 a + func (a+1) in (func v1)
                             in eval e 0

(* problem 6*)
type mobile = branch * branch   (* left and rigth branches *)
and branch = SimpleBranch of length * weight
          | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> let rec eval exp  = match exp with
                            | (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
                            | (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> w1+(eval m2)
                            | (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> (eval m1) + w2
                            | (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> (eval m1) + (eval m2)
          in let rec test exp = match exp with
                          | (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if l1*w1 != l2*w2 then false
                          else true
                          | (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> if l1*w1 != l2*(eval m2) then false
                          else test m2
                          | (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> if l1*(eval m1) != l2*w2 then false
                          else test m1
                          | (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> if l1*(eval m1) != l2*(eval m2) then false
                          else test m1 && test m2
                            in test m

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> let eval_digit a =( match a with
                                | ZERO -> 0
                                | ONE -> 1)
              in let rec rev a = match a with
                                | hd::tl -> rev tl@[hd]
                                | [] -> []
              in let rec eval exp num = (match exp with
                                    | hd::tl -> (eval_digit hd)*num+eval tl (num*2)
                                    | [] -> 0)
              in let rec rep num = if num = 0 then [ZERO]
              
                                  else if num - (num/2)*2 = 1 then if num/2 = 0 then ONE::[] else ONE::rep (num/2)
  else if num/2 = 0 then ZERO::[] else ZERO::rep (num/2)
             in rev (rep ((eval (rev b1) 1) * (eval (rev b2) 1)))

