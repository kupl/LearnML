let rec f : int -> int
 = fun x -> 
   if let c = x/10 in 
     c < 10 then 1
    else let c = x/10 in 1 + f c;;

let rec length l =
match l with
| [] -> 0
| hd::tl -> f hd + length tl;;

let int_exp x y = (float_of_int x) ** (float_of_int y) |> int_of_float;;

let rec lst2int : int list -> int
= fun lst -> (*TODO*)
    match lst with
      | [] -> 0
      | hd::tl -> let c = length lst in (int_exp 10 (c-1))*hd + lst2int tl;;
      
lst2int [5;2;3;4;5];;