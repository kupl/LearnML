(* problem 7*)
type digit = ZERO | ONE
type bin = digit list;;

let bmul : bin -> bin -> bin
= fun b1 b2 -> [ONE];;

let find_value = false;;


let rec add_bi x y =
match x with
[] -> []
| hd::tl ->
if List.length x > List.length y then [(hd, ZERO)]@add_bi tl y
else
match y with
[] -> []
| yh::yt -> [(hd,yh)]@ add_bi tl yt;;

let rec merge x =
match x with
[] -> []
| hd::tl ->
if hd = (ONE, ONE) then 2::merge tl
else if hd = (ONE, ZERO) || hd = (ZERO, ONE) then 1::merge tl
else 0::merge tl;;

let conver l =
match l with
[] -> []
| hd::tl -> if hd = 2 then [1;0]@tl
else hd::tl;;


