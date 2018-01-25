(* 2011210039 Kang Seungwoo *)

exception Problem;;

(* Problem 1 *)

let rec pascal (x,y)=
if y=0 then 1
else if x=y then 1
else if x<y then raise Problem
else pascal(x-1,y) + pascal(x-1,y-1);;

