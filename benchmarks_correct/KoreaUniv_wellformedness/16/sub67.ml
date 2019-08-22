
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string


  let rec compare : lambda -> string list -> bool
  = fun lambda lst ->
    match lambda with
    | V x -> 
			begin
      match lst with
      | [] -> false
      | hd::tl -> if x = hd then true
                  else compare lambda tl
			end
    | P (x, lambda1) -> compare lambda1 (x::lst)
		| C (lambda1, lambda2) -> (compare lambda1 lst) && (compare lambda2 lst)


  let rec check : lambda -> bool
  = fun lambda ->
		let lst = [] in   
		match lambda with
		| V x -> false
		| P (x, lambda1) -> compare lambda1 (x::lst)
		| C (lambda1, lambda2) -> (compare lambda1 lst) && (compare lambda2 lst)

