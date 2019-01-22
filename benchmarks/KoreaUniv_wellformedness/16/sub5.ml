
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string


  let rec comp : exp -> string list -> bool
  = fun exp lst ->
    match exp with
    | V x -> 
			begin
      match lst with
      | [] -> false
      | hd::tl -> if x = hd then true
                  else comp exp tl
			end
    | P (x, expa) -> comp expa (x::lst)
		| C (expa, expb) -> (comp expa lst) && (comp expb lst)


  let rec check : exp -> bool
  = fun exp ->
		let lst = [] in   
		match exp with
		| V x -> false
		| P (x, expa) -> comp expa (x::lst)
		| C (expa, expb) -> (comp expa lst) && (comp expb lst)

