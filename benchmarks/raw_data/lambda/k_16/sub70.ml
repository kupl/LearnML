
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec comparelist : 'a list -> 'a list -> 'a list
  = fun procdata vardata -> match (procdata, vardata) with
	  (_,[])   -> []
  | (hd1::tl1, hd2::tl2) -> if hd1 = hd2 then comparelist procdata tl2
    else (comparelist [hd1] tl2)@(comparelist tl1 vardata)
	| ([],hd2::tl2)       -> hd2::tl2;; 
	
  let rec varlist : lambda -> var list
  = fun lambda ->  match lambda with
	| V v     -> [v]
  | P (_, a) -> varlist a
  | C (a, b) -> varlist a@varlist b;;

  let rec proclist : lambda -> var list
	= fun lambda -> match lambda with
	| P (v, a) -> v::proclist a
	| C (a, b) -> proclist a@proclist b
  |_         -> [];;

  let check : lambda -> bool
  = fun lambda -> let list1 = proclist lambda in
		            let list2 = varlist lambda in
								 let list3 = comparelist list1 list2 in
										if list3 = [] then true
										else false;;
