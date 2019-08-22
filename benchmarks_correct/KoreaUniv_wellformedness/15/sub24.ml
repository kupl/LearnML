type lambda = V of var
	  | P of var * lambda
          | C of lambda * lambda
              and var = string
  
let check : lambda -> bool
  =fun e -> let rec check x y =
		match x with    
		|V x1 -> List.mem x1 y
		|C (x1, y1) -> check x1 y && check y1 y 
		|P (x1, y1) -> check y1 (x1::y) in check e[]



