type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list

exception InvalidArgument

let rec diff (aexpxpr, x) =
  match aexpxpr with
    Const y -> (Const 0)
  | Var y -> if x=y then (Const 1) else (Const 0)
  | Power(y, n) -> if (x=y) then (Times [(Const n); (Power(y, n-1))])
  				   else (Const 0)
  | Times l -> let len = (List.length l) in
				 if len=0 then raise InvalidArgument
  			     else let hd = (List.hd l) in
					  let tl = (List.tl l) in
 			   	 		if len=1 then (diff (hd, x))
						else (Sum [(Times (List.append [(diff ((Times [hd]), x))] tl)); (Times [hd; (diff ((Times tl), x))])])
  | Sum l -> let len = (List.length l) in
  			   if len=0 then raise InvalidArgument
  			   else let hd = (List.hd l) in
					let tl = (List.tl l) in
  					  if len=1 then (diff (hd, x))
					  else (Sum [(diff (hd, x)); (diff ((Sum tl), x))])
