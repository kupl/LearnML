type ae = CONST of int
	|VAR of string
	|POWER of string * int
	|TIMES of ae list
	|SUM of ae list

exception InvalidArgument

let rec diff (e, str) =
	match e with
	|CONST i -> CONST 0
	|VAR str1 -> if str1 = str then CONST 1
		     else CONST 0
	|POWER (str1,i) -> if i = 0 then CONST 0
		           else if str1 = str && i = 1 then CONST 1
			   else TIMES [CONST i; POWER(str1, i-1)] 
	|SUM [] -> raise InvalidArgument
	|SUM lst -> if List.length lst = 1 then diff (List.hd lst,str)
		    else SUM ((diff (List.hd lst,str)) ::  [(diff (SUM (List.tl lst), str))])
	|TIMES [] -> raise InvalidArgument
	|TIMES lst -> if List.length lst = 1 then diff (List.hd lst, str)
		      else SUM [TIMES (diff (List.hd lst,str) :: (List.tl lst));
			       TIMES [(List.hd lst) ; (diff (TIMES (List.tl lst), str))]]
				       
				
