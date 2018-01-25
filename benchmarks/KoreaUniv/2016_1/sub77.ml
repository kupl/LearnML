(* Problem 1 *)
let rec fib : int -> int
= fun n ->
    if n < 0 then 0 else
    let rec loop n a b = 
        if n = 0 
            then b
            else loop (n - 1) b (a + b) in 
    loop n 1 0

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
    let rec loop n r c =
        if n2 >= r
            then loop (n - 1) (r + 1) (c * n / r)
            else c in
    loop n1 1 1 (* because pascal is combination *)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> 
    if n <= 1 then false else
    let rec check_in_plist p plist =
        match plist with
        |   [] -> true
        |   h :: t -> if p mod h = 0 then false else check_in_plist p t in
    let rec loop i s plist = 
        if n > i
            then loop (i + s) 
                      (if s = 2 then 4 else 2)
                      (if check_in_plist i plist then i :: plist else plist)
            else check_in_plist n plist in
    match n with 
    |   1 | 4 -> false
    |   2 | 3 -> true
    |   _ -> loop 5 2 [3; 2]

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
    if a > b then sigma f b a else
    let rec loop a r =
        if a = b 
            then r + f a
            else loop (a + 1) (r + f a) in
    loop a 0