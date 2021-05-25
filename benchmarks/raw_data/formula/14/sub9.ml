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
;;

let rec calc exp =
    match exp with
    | Num (exp) -> exp
    | Plus (lExpr, rExpr) -> calc lExpr + calc rExpr
    | Minus (lExpr, rExpr) -> calc lExpr - calc rExpr
    ;;


let rec eval formula =
    match formula with
    | True -> true
    | False -> false
    | Not (formula) -> not (eval formula)
    | AndAlso (lFormula, rFormula) -> eval lFormula && eval rFormula
    | OrElse (lFormula, rFormula) -> eval lFormula || eval rFormula
    | Imply (lFormula, rFormula) -> eval (OrElse ((Not lFormula), rFormula))
    | Equal (lExpr, rExpr) -> calc lExpr = calc rExpr
    ;;

(*
print_endline (string_of_int (calc (Plus (Num 1, Num 2))));;
print_endline (string_of_int (calc (Minus (Num 2, Num 3))));;
print_endline (string_of_bool(eval (Equal (Num 4, Num 2))));
print_endline (string_of_bool (eval (Equal ((Minus (Num 1, Num 2)), (Minus (Num 1, Num 1))))));;

print_endline (string_of_bool (eval (True)));;
print_endline (string_of_bool (eval (False)));;
print_endline (string_of_bool (eval (Not True)));;
print_endline (string_of_bool (eval (Not False)));;
print_endline (string_of_bool (eval (AndAlso (True, True))));;
print_endline (string_of_bool (eval (AndAlso (True, False))));;
print_endline (string_of_bool (eval (AndAlso (False, True))));;
print_endline (string_of_bool (eval (AndAlso (False, False))));;
print_endline (string_of_bool (eval (OrElse (True, True))));;
print_endline (string_of_bool (eval (OrElse (True, False))));;
print_endline (string_of_bool (eval (OrElse (False, True))));;
print_endline (string_of_bool (eval (OrElse (False, False))));;
print_endline (string_of_bool (eval (Imply (True, True))));;
print_endline (string_of_bool (eval (Imply (True, False))));;
print_endline (string_of_bool (eval (Imply (False, True))));;
print_endline (string_of_bool (eval (Imply (False, False))));;
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
print_bool (true = eval True); 
print_bool (false = eval False); 
print_bool (false = eval (Not True)); 
print_bool (true = eval (Not False)); 
print_bool (true = eval (AndAlso (True, True))); 
print_bool (false = eval (AndAlso (True, False))); 
print_bool (false = eval (AndAlso (False, True))); 
print_bool (false = eval (AndAlso (False, False))); 
print_bool (true = eval (OrElse (True, True))); 
print_bool (true = eval (OrElse (True, False))); 
print_bool (true = eval (OrElse (False, True))); 
print_bool (false = eval (OrElse (False, False))); 
print_bool (false = eval (Imply (True, False))); 
print_bool (true = eval (Imply (True, True))); 
print_bool (true = eval (Imply (False, True))); 
print_bool (true = eval (Imply (False, False))); 
print_bool (true = eval (Equal (Num 3, Num 5))); 
print_bool (false = eval (Equal (Num 3, Num 3))); 
print_bool (false = eval (Equal (Num 3, Num 1))); 
print_bool (false = eval 
(Equal (Plus (Num 3, Num 4), Minus (Num 5, Num 1)))); 
print_bool (true = eval 
(Equal (Plus (Num 10, Num 12), Minus (Num 10, Num (-13))))); 
*)
