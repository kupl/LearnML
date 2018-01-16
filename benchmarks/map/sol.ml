let rec f func l =
	match l with
	|[] -> []
	|hd::tl -> (func hd) :: (f func tl) ;;
