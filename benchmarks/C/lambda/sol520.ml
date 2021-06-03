
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string


  let rec comp : lambda -> string list -> bool
  = fun lambda lst ->
    match lambda with
    | V x -> 
			begin
      match lst with
      | [] -> false
      | hd::tl -> if x = hd then true
                  else comp lambda tl
			end
    | P (x, lambdaa) -> comp lambdaa (x::lst)
		| C (lambdaa, lambdab) -> (comp lambdaa lst) && (comp lambdab lst)


  let rec check : lambda -> bool
  = fun lambda ->
		let lst = [] in   
		match lambda with
		| V x -> false
		| P (x, lambdaa) -> comp lambdaa (x::lst)
		| C (lambdaa, lambdab) -> (comp lambdaa lst) && (comp lambdab lst)

