(*
 * Brief      : HW1, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 12, 2014
 *)

(* Exercise 1 *)
let rec sigma f a b  =
  if a > b then 0
 	else if a = b then (f a) else (f b) + sigma f a (b-1)

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
