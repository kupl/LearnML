let rec f : int -> int -> int -> int -> int
= fun func a b -> if func a=func b then func a
else func a + f func (a+1) b ;;
