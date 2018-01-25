let rec fastexpt : int -> int -> int 
	= fun b n -> if (n = 0) then 1 
	else (if ((n mod 2) = 1) then b * (fastexpt b (n-1))
	else (fastexpt b (n/2)) * (fastexpt b (n/2)));;

let a = fastexpt 2 5;;

let rec find_root a n = if (a > (n*n)) then (find_root a (n+1)) else n;;
let rec find_divisor a n = if ((a mod n) = 0) then n else (if (n > (find_root a 1)) then a else (find_divisor a (n+1)));;

let smallest_divisor : int -> int = fun a -> find_divisor a 2;;

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if (n = 0) then (fun x -> x) else  (if (n=1) then f else (fun x -> (f (iter(n-1,f) x)))) ;;

let a = iter (2,fun x -> 2+x) 1;;

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if (a>b) then 1 else ((f a)*(product f (a+1) b));;

let dfact : int -> int
= fun n -> if (n mod 2 = 1) then (product (fun x -> ((2*x) - 1)) 1 ((n+1)/2)) else product (fun x -> (2*x)) 1 (n/2);;

let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with 
				| [] -> []
				| _::t -> if (n=1) then t else (drop t (n-1));;

let fst (x,_) = x;;
let snd (_,x) = x;;

let rec unzip_fst lst = match lst with
							| [] -> []
							| h::t -> ((fst h)::(unzip_fst t));;
let rec unzip_snd lst = match lst with
							| [] -> []
							| h::t -> ((snd h)::(unzip_snd t));;
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> ((unzip_fst lst),(unzip_snd lst));;

let isnil l 
	= match l with
		|[] -> true
		|_ -> false;;

let rec reverse_list lst 
	= match lst with
		| [] -> []
		| h::t -> (reverse_list t)@[h];;

let rec loop coins amount sum n 
	= match coins with
		| h::t -> if (n = -2) then (loop coins amount 0 (amount/h)) else (if(isnil t) 
			then (if (sum > amount )
				then 0 
				else 1) 
			else (if (n = -1)
				then 0
				else (loop coins amount sum (n-1))+(loop t amount (sum+(n*h)) (amount/h))));;

let change : int list -> int -> int
= fun coins amount -> if (amount>0) then (loop (reverse_list coins) amount 0 -2) else (if (amount = 0) then 1 else 0);;




