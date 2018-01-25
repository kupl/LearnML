type formula = 
True 
|False 
|Neg of formula 
|Or of formula * formula 
|And of formula * formula 
|Imply of formula * formula 
|Equiv of formula * formula 
let rec eval : formula -> bool =fun f -> true (* TODO *)
 |True -> true
 |False -> false
 |Negation c -> not (eval f c)
 |Disjunction (a, b) -> (eval f a) || (eval f b)
 |Conjunction (a, b) -> (eval f a) && (eval f b)
 |Implication (a, b) -> not (eval f a) || (eval f b)
 |Equivalence (a, b) -> (eval f a) = (eval f b)
