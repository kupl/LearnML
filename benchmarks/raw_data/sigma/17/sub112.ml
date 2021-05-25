let rec sum_list fn = 
	function
    | [] -> 0
    | hd :: tl -> fn hd + sum_list fn tl ;;

let rec make_list a b= 
	if (a<=b) then a::make_list (a+1) b 
	else [] ;;

let rec sigma ((a:int),(b:int),(fn:int->int)) = 
	sum_list fn (make_list a b);;
