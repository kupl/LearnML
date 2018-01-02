let rec f func a b = 
  	if func a != func b then let induction = func b in induction + f func a (b-1) 
  	else func b ;;    