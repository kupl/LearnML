let sum = 0 ;;
let rec sigma f a b =
	if a > b then sum
	else sum + f a + sigma f (a+1) b
;;

