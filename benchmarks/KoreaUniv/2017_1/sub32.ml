(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n ->
        if n = 0 then 1
        else if n mod 2 = 0 then let x = fastexpt b (n/2) in x*x
        else b * (fastexpt b (n-1))

(* problem 2*)

let rec smallest_divisor_rec i n =
        if i*i > n then n
        else if n mod i = 0 then i
        else smallest_divisor_rec (i+1) n

let smallest_divisor : int -> int
= fun n -> smallest_divisor_rec 2 n


(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
        match n with
        | 0 -> fun x -> x
        | _ -> fun x -> f (iter(n-1, f) x)

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
        if a=b then f a
        else (f a) * (product f (a+1) b)

(* problem 5*)

let dfact : int -> int
= fun n -> 
        match n mod 2 with
        | 0 -> product (fun x->2*x) 1 (n/2)
        | 1 -> product (fun x->2*x-1) 1 ((n+1)/2)
        | _ -> 0

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
        match n with
        | 0 -> l
        | _ -> match l with
                | [] -> l
                | hd::tl -> drop tl (n-1)

(* problem 7*)

let rec unzip_rec lst rst =
        match lst with
        | [] -> rst
        | hd::tl -> let (head_a, head_b) = hd in
                let (rst_a, rst_b) = rst in
                unzip_rec tl (rst_a@[head_a], rst_b@[head_b])

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> unzip_rec lst ([],[])
        
(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
        if amount < 0 then 0
        else if amount = 0 then 1
        else match coins with
                | [] -> 0
                | hd::tl -> (change tl amount) + (change coins (amount-hd))

