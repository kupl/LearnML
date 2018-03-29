(* problem 8*)

let rec length l =
	match l with
	|[] -> 0
	|hd::tl -> 1+(length tl)

let rec nth l n =
	match l with
	|[] -> raise (Failure "list is too short")
	|hd::tl -> if n = 0 then hd
			   else nth tl (n-1)

let rec reverse l =
	match l with
	|[] -> []
	|hd::tl -> (reverse tl)@[hd]

let rec insert a l =
	match l with
	|[] -> [a]
	|hd::tl -> if a < hd then a::hd::tl
			   else hd::(insert a tl)

let rec list_sort lst =
	match lst with
	|[] -> []
	|hd::tl -> insert hd (list_sort tl)

let descending_sort lst = reverse(list_sort(lst))

let rec count_options (coins, amount, index) =
	if amount = 0 then 1
	else if (amount < 0)||(index >= length(coins)) then 0
	else count_options(coins, amount, (index+1)) + count_options(coins, (amount - nth coins index), index)  

let change : int list -> int -> int
= fun coins amount -> 
	count_options (descending_sort coins, amount, 0)



