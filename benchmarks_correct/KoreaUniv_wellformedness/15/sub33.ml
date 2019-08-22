  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string

  
  let check : lambda -> bool
    =fun e ->
      	let rec varArr x y_list =
        		match x with
          		|V a -> List.mem a y_list
          		|P (a, b) -> varArr b (a::y_list)
          		|C (a, b) -> varArr a y_list && varArr b y_list
      	in
        	varArr e []
