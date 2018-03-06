let rec filter p = function
| [] -> []
| hd :: tl -> if (p hd) then hd :: filter p tl else filter p tl
;;
