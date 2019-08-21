type aexp = Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

exception InvalidArgument

let rec diff (e, str) =
	match e with
	|Const i -> Const 0
	|Var str1 -> if str1 = str then Const 1
		     else Const 0
	|Power (str1,i) -> if i = 0 then Const 0
		           else if str1 = str && i = 1 then Const 1
			   else Times [Const i; Power(str1, i-1)] 
	|Sum [] -> raise InvalidArgument
	|Sum lst -> if List.length lst = 1 then diff (List.hd lst,str)
		    else Sum ((diff (List.hd lst,str)) ::  [(diff (Sum (List.tl lst), str))])
	|Times [] -> raise InvalidArgument
	|Times lst -> if List.length lst = 1 then diff (List.hd lst, str)
		      else Sum [Times (diff (List.hd lst,str) :: (List.tl lst));
			       Times [(List.hd lst) ; (diff (Times (List.tl lst), str))]]
				       
				
