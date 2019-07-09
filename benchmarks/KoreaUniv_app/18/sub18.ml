let rec is_in : 'a list -> 'a -> bool
=fun lst a -> match lst with []->false | hd::tl -> if a=hd then true else (is_in tl a);;

is_in [1;2;3] 1;;
is_in [1;2;3] 4;;


let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l1 with [] -> l2 | hd::tl -> if is_in l2 hd then app tl l2 else app tl (l2@[hd]);;

app [1;2;3;4] [2;3;4;6];;
app [4;5;6;7] [1;2;3;4];;