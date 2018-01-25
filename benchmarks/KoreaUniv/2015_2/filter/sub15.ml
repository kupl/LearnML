(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = [] 
(module LinkedList = 
 let filter pred list = 
         let folder head tail =
            if pred head then 
                Cons(head,tail)
            else
                tail)
