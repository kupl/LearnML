let sum = 0 ;;
let rec sigma ( a, b, f ) =
	if a > b then sum
	else sum + f a + sigma ( a+1, b, f )
;;

