type var = string

type exp =
  | V of var
  | P of var * exp
  | C of exp * exp

  let rec comparelist : 'a list -> 'a list -> 'a list
  = fun procdata vardata -> match (procdata, vardata) with
	  (_,[])   -> []
  | (hd1::tl1, hd2::tl2) -> if hd1 = hd2 then comparelist procdata tl2
    else (comparelist [hd1] tl2)@(comparelist tl1 vardata)
	| ([],hd2::tl2)       -> hd2::tl2;; 
	
  let rec varlist : exp -> var list
  = fun exp ->  match exp with
	| V v     -> [v]
  | P (_, a) -> varlist a
  | C (a, b) -> varlist a@varlist b;;

  let rec proclist : exp -> var list
	= fun exp -> match exp with
	| P (v, a) -> v::proclist a
	| C (a, b) -> proclist a@proclist b
  |_         -> [];;

  let check : exp -> bool
  = fun exp -> let list1 = proclist exp in
		            let list2 = varlist exp in
								 let list3 = comparelist list1 list2 in
										if list3 = [] then true
										else false;;