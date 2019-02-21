(*********************)
(*     Problem 1     *)
(*********************)
let bigger x y =
if x > y then x
else y ;;
let rec fold bigger l a =
match l with
| [] -> a
| hd::tl -> bigger hd (fold bigger tl a) ;;
let rec max : int list -> int
= fun lst -> fold bigger lst 0 ;;

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
let rec min : int list -> int
= fun lst -> fold smaller lst (last lst) ;;
