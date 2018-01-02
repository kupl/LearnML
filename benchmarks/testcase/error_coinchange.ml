let rec insert a l = 
  match l with
  | [] -> [a]
  | hd::tl -> if a>hd then a::hd::tl
            else hd::(insert a tl) ;;

let rec sort l = 
  match l with
  | [] -> []
  | hd::tl -> insert hd (sort tl) ;;

let rec calc coins amount first =
  match coins with
	| [] -> 0
  | hd::tl -> if (amount=first) then 1
				else if (tl = []) then (if amount mod hd = 0 then 1 else 0)
				else if (amount>first) then (calc coins (amount-first) hd)+(calc tl amount first)
 	      else 0 ;;

let rec f : int list -> int -> int
= fun coins amount -> 
  let sorted_coins = (sort coins) in 
   (
    if amount<0 then 0
    else if amount=0 then 1
    else 
      match sorted_coins with
      | [] -> 0
      | hd::tl -> ((calc sorted_coins amount hd)+(f tl amount))
   )
;;
