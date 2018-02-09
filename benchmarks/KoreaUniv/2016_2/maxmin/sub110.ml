(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int= fun lst ->
match lst with
|[] -> failwith "None"
|h::t ->  
let rec helper (seen,rest) =
match rest with 
|[] -> seen
|h'::t' -> let seen' =
if h' > seen then h' else seen in let rest' = t' 
in helper (seen',rest') in helper (h,t) 

let rec min : int list -> int= fun lst ->
match lst with
|[] -> failwith "None"
|h::t ->  
let rec helper (seen,rest) =
match rest with 
|[] -> seen
|h'::t' -> let seen' =
if h' < seen then h' else seen in let rest' = t' 
in helper (seen',rest') in helper (h,t) 