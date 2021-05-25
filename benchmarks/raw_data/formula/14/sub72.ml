(*
 * Brief      : HW1, Program Language (4190.310)
 * Author     : YongKi Kim =kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 12, 2014
 *)

type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp

(* Exercise 2 *)
let rec eval : formula -> bool =
  let rec eval_exp : exp -> int = function
   	| Num n 	-> n
   	| Plus  (e1,e2)	-> (eval_exp e1) + (eval_exp e2)
   	| Minus (e1,e2) -> (eval_exp e1) - (eval_exp e2)
  in function
    | True    -> true
    | False   -> false
    | Not f   -> let v = eval f in if v then false else true
    | AndAlso (f1,f2) -> let (v1, v2) = ((eval f1), (eval f2)) in
  		  if v1 && v2 then true else false
    | OrElse  (f1,f2) -> let (v1, v2) = ((eval f1), (eval f2)) in
  		  if v1 || v2 then true else false
    | Imply   (f1,f2) -> let (v1, v2) = ((eval f1), (eval f2)) in
  		  if v1 && v2 = false then false else true
    | Equal    (e1,e2) -> let (v1, v2) = ((eval_exp e1), (eval_exp e2)) in
  		  if v1 = v2 then true else false
