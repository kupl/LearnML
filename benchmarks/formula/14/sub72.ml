(*
 * Brief      : HW1, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 12, 2014
 *)

type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

(* Exercise 2 *)
let rec eval : formula -> bool =
  let rec eval_expr : expr -> int = function
   	| NUM n 	-> n
   	| PLUS  (e1,e2)	-> (eval_expr e1) + (eval_expr e2)
   	| MINUS (e1,e2) -> (eval_expr e1) - (eval_expr e2)
  in function
    | TRUE    -> true
    | FALSE   -> false
    | NOT f   -> let v = eval f in if v then false else true
    | ANDALSO (f1,f2) -> let (v1, v2) = ((eval f1), (eval f2)) in
  		  if v1 && v2 then true else false
    | ORELSE  (f1,f2) -> let (v1, v2) = ((eval f1), (eval f2)) in
  		  if v1 || v2 then true else false
    | IMPLY   (f1,f2) -> let (v1, v2) = ((eval f1), (eval f2)) in
  		  if v1 && v2 = false then false else true
    | LESS    (e1,e2) -> let (v1, v2) = ((eval_expr e1), (eval_expr e2)) in
  		  if v1 < v2 then true else false
