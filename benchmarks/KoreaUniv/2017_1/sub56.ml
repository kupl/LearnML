(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n -> if n=0 then 1
             else if n=1 then b
             else if n mod 2 = 0 then (fastexpt b (n/2))*(fastexpt b (n/2))
             else b*(fastexpt b (n-1))

(* problem 2*)

exception Problem

let rec findPrime : int -> int -> int
= fun n p -> if (p*p)>n then n
             else if n mod p = 0 then p
             else (findPrime n (p+1))

let smallest_divisor : int -> int
= fun n -> if (n<=0) then raise Problem
           else if (n=1) then 1
           else if n mod 2 = 0 then 2
           else findPrime n 3

(* problem 3*)

let compose f g = fun x -> f(g x)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n<0 then raise Problem
               else if n=0 then (fun x -> x)
               else compose f (iter ((n-1),  f))

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a>b then raise Problem
               else if a=b then f a
               else (f a)*(product f (a+1) b)

(* problem 5*)

let dfact : int -> int
= fun n -> if n=0 then 0
           else if n mod 2 = 0 then product (fun x -> (2*x)) 1 (n/2)
           else product (fun x -> (2*x)-1) 1 ((n+1)/2)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> if n<0 then l
             else if n=0 then l
             else match l with
             | [] -> []
             | hd::tl -> drop tl (n-1)

(* problem 7*)

let rec tuple_value
= fun f lst -> match lst with
             | [] -> []
             | hd::tl -> (f hd)::(tuple_value f tl)


let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
         | [] -> raise Problem
         | _ -> (tuple_value (fun (x,_)->x) lst, tuple_value (fun (_,x)->x) lst)



(* problem 8*)

let rec insert a l = 
match l with
| [] -> [a]
| hd::tl -> if a>hd then a::hd::tl
            else hd::(insert a tl)

let rec sort l = 
match l with
| [] -> []
| hd::tl -> insert hd (sort tl)

let rec calc
= fun coins amount first -> match coins with
		            | [] -> 0
                            | hd::tl -> if (amount=first) then 1
					else if (tl = []) then (if amount mod hd = 0 then 1 else 0)
					else if (amount>first) then (calc coins (amount-first) hd)+(calc tl amount first)
 				        else 0

let rec change : int list -> int -> int
= fun coins amount -> let sorted_coins = (sort coins) in 
                      (if amount<0 then 0
                      else if amount=0 then 1
                      else match sorted_coins with
                           | [] -> 0
                           | hd::tl -> ((calc sorted_coins amount hd)+(change tl amount)))




