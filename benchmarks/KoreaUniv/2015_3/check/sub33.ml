  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  
  let check : exp -> bool
    =fun e ->
      	let rec varArr x y_list =
        		match x with
          		|V a -> List.mem a y_list
          		|P (a, b) -> varArr b (a::y_list)
          		|C (a, b) -> varArr a y_list && varArr b y_list
      	in
        	varArr e []
