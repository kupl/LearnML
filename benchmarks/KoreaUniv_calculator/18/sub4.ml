type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
let rec range : int -> int -> int list = fun n1 n2 ->
  if n1 = n2 then n1::[] 
  else n1 :: (range (n1+1) (n2-1)) @ [n2];;
  
let rec sigma : (int -> int) -> int list -> int
= fun f l ->
  match l with
    | [] -> f 0
    | hd::tl -> (f hd) + (sigma f tl);;
    
let rec sigma2 : int list -> int
= fun l ->
  match l with
    | [] -> 0
    | hd::tl -> hd + sigma tl;;
    
    
let varToval : exp -> int -> int
= fun e x ->
  if e = X then x else 0;;

(*let rec expTointFun : exp -> (int -> int)
= fun e ->
  match e with
    | SUB (a, b) -> if a = X && b <> X then fun x -> x - (calculator b)
                    else if a <> X && b = X then fun x -> x - (calculator a)
                    else if a = X && b = X then fun x -> x - x
                    
    | ADD (a, b) -> if a = X && b <> X then fun x -> x + (calculator b)
                    else if a <> X && b = X then fun x -> x + (calculator a)
                    else if a = X && b = X then fun x -> x + x
                    
    | MUL (a, b) -> if a = X && b <> X then fun x -> x * (calculator b)
                    else if a <> X && b = X then fun x -> x * (calculator a)
                    else if a = X && b = X then fun x -> x * x

    | DIV (a, b) -> if a = X && b <> X then fun x -> x / (calculator b)
                    else if a <> X && b = X then fun x -> x / (calculator a)
                    else if a = X && b = X then fun x -> x / x
                    *)
let rec X_for_sigma2 : exp -> int -> int
= fun ex x ->
  match ex with
    | INT (n) -> n
    | SUB (ex1, ex2) -> if ex1 = X && ex2 <> X then (x) - (X_for_sigma2 ex2)
                        else if ex1 <> X && ex2 = X then (X_for_sigma2 ex1) - (x)
                        else if ex1 = X && ex2 = X then 0
                        else (X_for_sigma2 ex1) - (X_for_sigma2 ex2)
                        
    | ADD (ex1, ex2) -> if ex1 = X && ex2 <> X then (x) + (X_for_sigma2 ex2)
                        else if ex1 <> X && ex2 = X then (X_for_sigma2 ex1) + (x)
                        else if ex1 = X && ex2 = X then ADD(Int x, Int x)
                        else (X_for_sigma2 ex1) + (X_for_sigma2 ex2)
                        
    | MUL (ex1, ex2) -> if ex1 = X && ex2 <> X then (x) * (X_for_sigma2 ex2)
                        else if ex1 <> X && ex2 = X then (X_for_sigma2 ex1) * (x)
                        else if ex1 = X && ex2 = X then MUL(Int x, Int x)
                        else (X_for_sigma2 ex1) * (X_for_sigma2 ex2)
                        
    | DIV (ex1, ex2) -> if ex1 = X && ex2 <> X then (x) / (X_for_sigma2 ex2)
                        else if ex1 <> X && ex2 = X then (X_for_sigma2 ex1) / (x)
                        else if ex1 = X && ex2 = X then 1
                        else (X_for_sigma2 ex1) / (X_for_sigma2 ex2);;
                      
let rec list_for_sigma2 : exp -> int list -> int list
= fun ex l ->
  match l with
    | [] -> []
    | hd::tl -> let first = X_for_sigma2 ex hd
                in let less = list_for_sigma2 ex tl
                   in let final_list = first::less;;
                
                (*(X_for_sigma2 ex hd)::[list_for_sigma2 ex tl];;*)
                    

let rec calculator : exp -> int
= fun exp -> 
  match exp with
    | INT (x) -> x
    | SUB (x, y) -> (calculator x) - (calculator y)
                    
    | ADD (x, y) -> (calculator x) + (calculator y)
                    
    | MUL (x, y) ->  (calculator x) * (calculator y)
                    
    | DIV (x, y) -> (calculator x) / (calculator y)
                    
    | SIGMA (start, ended, ex) -> let st = calculator start
                                  in let ed = calculator ended
                                     in let range_list = range st ed
                                        in let sigma2_list = list_for_sigma2 ex range_list
                                           in sigma2 sigma2_list;;
                                  
                                 (*sigma2 (list_for_sigma2 ex (range (calculator start) (calculator ended))));;*)

    
    
    
    (*| (h, _ ) -> begin match h with
                     | INT (x) -> 
                     | MUL (a, b) -> if a = X && b = X then fun x -> x * x
                                     else if a = X && b <> X then fun x -> x * (calculator b)
                                     else if a <> X && b = X then fun x -> (calculator a) * x
                    
                     | DIV (a, b) -> if a = X && b = X then fun x -> 1
                                     else if a = X && b <> X then fun x -> x * (calculator b)
                                     else if a <> X && b = X then fun x -> (calculator a) * x
                    
                     | ADD (a, b) -> if a = X && b = X then fun x -> x + x
                                     else if a = X && b <> X then fun x -> x * (calculator b)
                                     else if a <> X && b = X then fun x -> (calculator a) * x
                    
                     | SUB (a, b) -> if a = X && b = X then fun x -> 0
                                     else if a = X && b <> X then fun x -> x * (calculator b)
                                     else if a <> X && b = X then fun x -> (calculator a) * x
                 end
                    
     | (_ , t ) -> begin match t with
                     | INT (x) -> 
                     | MUL (a, b) -> if a = X && b = X then fun x -> x * x
                                     else if a = X && b <> X then fun x -> x * (calculator b)
                                     else if a <> X && b = X then fun x -> (calculator a) * x
                    
                     | DIV (a, b) -> if a = X && b = X then fun x -> 1
                                     else if a = X && b <> X then fun x -> x * (calculator b)
                                     else if a <> X && b = X then fun x -> (calculator a) * x
                    
                     | ADD (a, b) -> if a = X && b = X then fun x -> x + x
                                     else if a = X && b <> X then fun x -> x * (calculator b)
                                     else if a <> X && b = X then fun x -> (calculator a) * x
                    
                     | SUB (a, b) -> if a = X && b = X then fun x -> 0
                                     else if a = X && b <> X then fun x -> x * (calculator b)
                                     else if a <> X && b = X then fun x -> (calculator a) * x
                  end*)
    