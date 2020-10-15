let rec sigma f a b  =
	if b>a then sigma f a (b-1)+f b
	else if b<a then 0
	else f a ;;

