let rec max lst = 
    match lst with 
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
(let rec min lst = 
    match lst with 
    | [] -> 0 
    | x :: [] -> x 
    | x :: xs ->  
          let v = min xs in 
          if x < v then 
             x 
          else 
             v ;;
