(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b c -> if c = 1 then b
              else if c mod 2 = 1 then b * (fastexpt b (c-1))
              else (fastexpt b (c/2)) * (fastexpt b (c/2))

(* problem 2*)
let rec help_smallest_divisor : int -> int -> int
= fun n a -> if float_of_int a > sqrt(float_of_int n) then n
             else if n mod a = 0 then a
             else (help_smallest_divisor n (a + 1))

let smallest_divisor : int -> int
= fun n -> if n = 1 then 1
           else if n mod 2 = 0 then 2
           else (help_smallest_divisor n 3)

(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n, f) -> if n = 0 then fun x -> x
                else let main_func = f 
                     in let rec help_iter n f = if n = 1 then f
                                                else help_iter (n-1) (fun x -> main_func(f(x)))
                        in (help_iter n f)

(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f a 
               else if a > b then 0
               else (f a) * (product f (a+1) b)

(* problem 5*)
let dfact : int -> int
= fun n -> if n mod 2 = 0 then (product (fun x -> x*2) 1 (n/2))
           else (product (fun x -> (x*2-1)) 1 ((n+1)/2))

(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> if n = 0 then l
             else match l with | [] -> l
                               | hd::tl -> (drop tl (n-1))

(* problem 7*)
let rec help_unzip1 : ('a * 'b) list -> 'a list
= fun lst -> match lst with | [] -> []
                            | hd::tl -> match hd with (x,_) -> x::(help_unzip1 tl)

let rec help_unzip2 : ('a * 'b) list -> 'b list
= fun lst -> match lst with | [] -> []    
                            | hd::tl -> match hd with (_,x) -> x::(help_unzip2 tl)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with | [] -> ([],[])
                            | hd::tl -> ((help_unzip1 lst) , (help_unzip2 lst))

(* problem 8*)
let rec change : int list -> int -> int
= fun (coins) (amount) -> if amount = 0 then 1
                      else if amount < 0 then 0
                      else match coins with
                             | [] -> 0
                             | hd::tl -> if amount < hd then (change tl amount)
                                         else (change tl amount) + (change coins (amount-hd))
