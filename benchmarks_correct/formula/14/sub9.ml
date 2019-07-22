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
;;

let rec calc expr =
    match expr with
    | NUM (expr) -> expr
    | PLUS (lExpr, rExpr) -> calc lExpr + calc rExpr
    | MINUS (lExpr, rExpr) -> calc lExpr - calc rExpr
    ;;


let rec eval formula =
    match formula with
    | TRUE -> true
    | FALSE -> false
    | NOT (formula) -> not (eval formula)
    | ANDALSO (lFormula, rFormula) -> eval lFormula && eval rFormula
    | ORELSE (lFormula, rFormula) -> eval lFormula || eval rFormula
    | IMPLY (lFormula, rFormula) -> eval (ORELSE ((NOT lFormula), rFormula))
    | LESS (lExpr, rExpr) -> calc lExpr < calc rExpr
    ;;

(*
print_endline (string_of_int (calc (PLUS (NUM 1, NUM 2))));;
print_endline (string_of_int (calc (MINUS (NUM 2, NUM 3))));;
print_endline (string_of_bool(eval (LESS (NUM 4, NUM 2))));
print_endline (string_of_bool (eval (LESS ((MINUS (NUM 1, NUM 2)), (MINUS (NUM 1, NUM 1))))));;

print_endline (string_of_bool (eval (TRUE)));;
print_endline (string_of_bool (eval (FALSE)));;
print_endline (string_of_bool (eval (NOT TRUE)));;
print_endline (string_of_bool (eval (NOT FALSE)));;
print_endline (string_of_bool (eval (ANDALSO (TRUE, TRUE))));;
print_endline (string_of_bool (eval (ANDALSO (TRUE, FALSE))));;
print_endline (string_of_bool (eval (ANDALSO (FALSE, TRUE))));;
print_endline (string_of_bool (eval (ANDALSO (FALSE, FALSE))));;
print_endline (string_of_bool (eval (ORELSE (TRUE, TRUE))));;
print_endline (string_of_bool (eval (ORELSE (TRUE, FALSE))));;
print_endline (string_of_bool (eval (ORELSE (FALSE, TRUE))));;
print_endline (string_of_bool (eval (ORELSE (FALSE, FALSE))));;
print_endline (string_of_bool (eval (IMPLY (TRUE, TRUE))));;
print_endline (string_of_bool (eval (IMPLY (TRUE, FALSE))));;
print_endline (string_of_bool (eval (IMPLY (FALSE, TRUE))));;
print_endline (string_of_bool (eval (IMPLY (FALSE, FALSE))));;
*)

(*
type tree =
    | Leaf of int
    | Tree of tree * tree
    ;;

let rec sum_of_tree tree =
    match tree with
    | Leaf value -> value
    | Tree (ltree, rtree) -> sum_of_tree(ltree) + sum_of_tree(rtree)
    ;;

print_endline (string_of_int (sum_of_tree (Tree (Tree (Leaf 3, Leaf 4), Tree (Tree (Leaf 3, Leaf 4), Leaf 5)))))
;;
print_endline (string_of_int (sum_of_tree (Leaf 5)))
;;

let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (true = eval TRUE); 
print_bool (false = eval FALSE); 
print_bool (false = eval (NOT TRUE)); 
print_bool (true = eval (NOT FALSE)); 
print_bool (true = eval (ANDALSO (TRUE, TRUE))); 
print_bool (false = eval (ANDALSO (TRUE, FALSE))); 
print_bool (false = eval (ANDALSO (FALSE, TRUE))); 
print_bool (false = eval (ANDALSO (FALSE, FALSE))); 
print_bool (true = eval (ORELSE (TRUE, TRUE))); 
print_bool (true = eval (ORELSE (TRUE, FALSE))); 
print_bool (true = eval (ORELSE (FALSE, TRUE))); 
print_bool (false = eval (ORELSE (FALSE, FALSE))); 
print_bool (false = eval (IMPLY (TRUE, FALSE))); 
print_bool (true = eval (IMPLY (TRUE, TRUE))); 
print_bool (true = eval (IMPLY (FALSE, TRUE))); 
print_bool (true = eval (IMPLY (FALSE, FALSE))); 
print_bool (true = eval (LESS (NUM 3, NUM 5))); 
print_bool (false = eval (LESS (NUM 3, NUM 3))); 
print_bool (false = eval (LESS (NUM 3, NUM 1))); 
print_bool (false = eval 
(LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
print_bool (true = eval 
(LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))))); 
*)
