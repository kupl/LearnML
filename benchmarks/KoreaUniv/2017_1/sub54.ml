(* problem 1*)
let rec fastexpt: int -> int -> int 
= fun b n -> if n = 1 then b 
    else match n mod 2 with
         0 -> fastexpt b (n/2) * fastexpt b (n/2)
        |_ -> b * (fastexpt b (n-1));;


(* problem 2*)
let rec smallest_divisor: int -> int
= fun n -> 
    let rec mini_divisor a b = 
        if a >= b * b then 
            match a mod b with
            |0 -> b
            |_ -> mini_divisor a (b+1)
        else a
    in mini_divisor n 2;;



(* problem 3*)
let rec iter: int * (int -> int) -> (int -> int)
    = fun (n, f) ->
    match n with
    | 0 -> (fun x -> x)
    | _ -> (let compose f g
          = fun x -> f(g(x)) in compose f (iter (n-1, f)));;



(* problem 4*)
let rec product: (int -> int) -> int -> int -> int
    = fun f a b -> if a = b then f a 
    else (f b) * (product f a (b-1));;



(* problem 5*)
let dfact: int -> int
= fun n -> let rec product: (int -> int) -> int -> int -> int
    = fun f a b -> if a = b then f a 
    else (f b) * (product f a (b-1)) in 
        match n mod 2 with
        | 0 -> product (fun x -> 2*x) 1 (n/2)
        | _ -> product (fun x -> 2*x - 1) 1 ((n+1)/2);;

(* problem 6*)
let rec drop: 'a list -> int -> 'a list
    = fun l n -> 
        match l with
        | [] -> []
        | hd :: tl -> match n with
                    | 0 -> l
                    | _ -> drop tl (n-1);;



(* problem 7*)
let unzip: ('a * 'b) list -> 'a list * 'b list
    = fun lst -> 
        let rec re_unzip: ('a * 'b) list -> ('a list * 'b list) -> 'a list * 'b list
        = fun lst (fls, sls) -> 
            match lst with
            | [] -> (fls, sls)
            | (x, y) :: tl -> re_unzip tl (fls @ [x], sls @ [y]) in
                re_unzip lst ([], []);;

(* problem 8*)
let change: int list -> int -> int
= fun coins amount -> let rec re_change: int list -> int -> int -> int -> int
                    = fun coins amount count value ->
                    match coins with
                    | [] -> 0
                    | [a] -> 1
                    | hd :: tl -> if count > value then 0
                                else 1 + (re_change coins (amount mod hd + hd*count) (count + 1) value) in
                    
                    let rec sort: int list -> int list
                    = fun l ->
                        match l with
                        | [] -> []
                        | hd :: tl -> let rec insert: int -> int list -> int list
                                        = fun a l -> 
                                        match l with
                                        | [] -> [a]
                                        | hd :: tl -> if hd > a then hd :: (insert a tl)
                                                    else a :: l in
                        insert hd (sort tl) in
                    match (sort coins) with
                    | [] -> 0
                    | [a] -> 1
                    | hd :: tl -> re_change tl amount 0 (amount / hd);;

