let smaller x y =
if x > y then y
else x ;;
let rec fold smaller l a =
match l with
| [] -> a
| hd::tl -> smaller hd (fold smaller tl a) ;;
let rec last l =
match l with
| [a] -> a
| _::tl -> last tl ;;
let rec f : int list -> int
= fun lst -> fold smaller lst (last lst) ;;
