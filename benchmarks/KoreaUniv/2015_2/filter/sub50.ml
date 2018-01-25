let rec filter p = function
| [] -> []
| hd :: tl when p hd -> hd :: filter p tl
| _ :: tl -> filter p tl
;;
