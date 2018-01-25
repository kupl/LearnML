let rec pascal : int * int -> int =fun (x,y) -> 1
    | 1 -> [1]
    | n when n < 1 -> failwith "pascal: invalid argument (row >= 1)"
    | n -> 
      let x = 0 :: pascal (row - 1) in
      let y = List.rev a in
      List.map2 (+) x y

let rec sigma f = function
    | [] -> 0
    | a :: b -> f a + sigma f b;;


let rec max list = 
    match list with 
    | [] -> 0 
    | x :: [] -> x 
    | x :: xs ->  
          let v = max xs in 
          if x > v then 
             x 
          else 
             v
;; 
let rec min : int list -> int =fun l -> 1 
(let rec min list = 
    match list with 
    | [] -> 0 
    | x :: [] -> x 
    | x :: xs ->  
          let v = min xs in 
          if x < v then 
             x 
          else 
             v ;;

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

type nat = ZERO | SUCC of nat;;
let rec natadd m : function
ZERO -> m
| SUCC n-> natadd (SUCC m) n;;

let rec natmul a b = match b with
ZERO -> ZERO
| SUCC n1 -> add (natmul a n) a;;