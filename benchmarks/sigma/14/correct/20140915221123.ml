let rec sigma (a, b, f) =
	if b>a then sigma(a,b-1,f)+f b
	else if b<a then 0
	else f a ;;

