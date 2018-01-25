(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if n = 0 then 1
             else if (n mod 2) = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
             else b * (fastexpt b (n-1))

(* problem 2*)

let rec smallest_divisor : int -> int
= fun n -> let rec find_divisor : int -> int -> int
            = fun n test_divisor -> if test_divisor * test_divisor > n then n
                                    else if (n mod test_divisor) = 0 then test_divisor
                                    else find_divisor n (test_divisor+1)
  in find_divisor n 2

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n = 0 then (fun x -> x)
              else if n = 1 then (fun x -> f x)
              else (fun x -> iter (n-1, f) (f x))

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a>b then raise (Failure "Error: b must be larger than a")
              else if a=b then (f a)
              else (f b) * (product f a (b-1))

(* problem 5*)

let rec dfact : int -> int
= fun n -> if n = 0 || n = 1 then 1
          else n * dfact(n-2)
          

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
    match l with
    | [] -> []
    | hd::t1 -> if n=0 then l
                else drop t1 (n-1)

(* problem 7*)

let fst (x,_) = x
let snd (_,x) = x

let rec make_first : ('a * 'b) list -> 'a list
= fun lst -> match lst with
            | [] -> []
            | hd::t1 -> (fst hd)::(make_first t1)
let rec make_second : ('a * 'b) list -> 'b list
= fun lst -> match lst with
            | [] -> []
            | hd::t1 -> (snd hd)::(make_second t1)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let firstlst = make_first lst 
            in let secondlst = make_second lst
            in (firstlst, secondlst)

            
(* problem 8*)

let rec kind_of_coins : int list -> int
= fun lst -> match lst with
          | [] -> 0
          | hd::t1 -> 1 + (kind_of_coins t1)

(* have to got biggest *)
let rec first_domination : int list -> int -> int
= fun lst kind_of_coins -> match lst with
                        | [] -> raise (Failure "list is too short")
                        | hd::t1 -> if kind_of_coins = 1 then hd
                                    else first_domination t1 (kind_of_coins-1)

let rec change : int list -> int -> int
= fun coins amount -> let rec cc  
                      = fun coins coins_kind_num amount -> if amount = 0 then 1
                                                         else if ((amount) < 0) || (coins_kind_num = 0) then 0
                                                         else cc coins (coins_kind_num-1) amount + cc coins coins_kind_num (amount- (first_domination coins coins_kind_num))
                      in cc coins (kind_of_coins coins) (amount)
