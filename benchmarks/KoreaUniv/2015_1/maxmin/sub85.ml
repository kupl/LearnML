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
