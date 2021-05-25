let rec is_in : 'a list -> 'a -> bool
=fun lst a -> match lst with []->false | hd::tl -> if a=hd then true else (is_in tl a);;

is_in [1;2;3] 1;;
is_in [1;2;3] 4;;

let rec unique : 'a list -> 'a list -> 'a list
=fun lst1 lst2-> match lst1 with [] -> lst2 | hd::tl -> if (is_in lst2 hd) then (unique tl lst2) else (unique tl (lst2@[hd]));;

let rec uniq : 'a list -> 'a list
= fun lst -> unique lst [] ;;

uniq [1;2;3;4;3];;
uniq[5;6;5;4];;