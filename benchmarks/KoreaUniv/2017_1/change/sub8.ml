(* problem 8*)


let rec tree lst sum amount= 
if sum=amount then 1
else if sum>amount then 0
else
 match lst with
 []->0
 |hd::tl -> (tree lst (sum+hd) amount)+ (tree tl sum amount);;

let change : int list -> int -> int
= fun coins amount ->

if coins = [] then 0
else if amount = 0 then 1
else if amount <0 then 0
else tree coins 0 amount;;


