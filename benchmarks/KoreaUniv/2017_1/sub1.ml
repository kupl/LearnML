let fastexpt : int -> int -> int
= fun b n -> let gop x = x * x in let rec fastexpta b n =
    if n = 0 then 1
    else
      if n mod 2 = 0 then  gop (fastexpta b (n/2))
      else b * (fastexpta b (n-1)) in fastexpta b n


let smallest_divisor : int -> int
 = fun n ->let rec smd y n = if n mod y = 0 then y else if y*y > n then n else smd (y+1) n in smd 2 n


let iter : int * (int->int) -> (int->int)
  = fun (n,f) -> let rec iters(n,f) x = if n = 0 then (fun n -> n) x else if n = 1 then f x else f (iters(n-1,f) x) in iters(n,f)

let product : (int->int) -> int -> int -> int
  = fun f a b -> let rec prod f a b = if a = b+1 then 1 else (f a)*(prod f (a+1) b) in prod f a b

let dfact : int->int
  = fun n -> let rec df n = if n < 3 then n else n * df (n-2) in df n

let drop : int list -> int -> int list = fun l n -> let rec dr l n = match l with | [] -> [] | hd::tl -> if n > 0 then dr tl (n-1) else hd::dr tl (n-1) in dr l n


let unzip : ('a * 'b) list -> 'a list * 'b list
  = fun lst -> let rec u lst = match lst with | [] -> [] | (y,v)::tl -> y::u tl
    in let rec z lst = match lst with | [] -> [] | (y,v)::tl -> v::z tl
      in let uz lst = match lst with | [] -> [],[] | (y,v)::tl -> u lst,z lst in uz lst

let change : int list -> int -> int 
  = fun coins amount -> let rec leng lst = match lst with | [] -> 0 | hd::tl -> 1+leng tl
  in let rec find_ele lst n = match lst with | [] -> 0 | hd::tl -> if n = 0 then hd else find_ele tl (n-1)
  in let rec ch coins amount idx = if amount = 0 then 1 else if amount < 0 || idx >= leng coins then 0 else (ch coins (amount-(find_ele coins idx)) idx) + (ch coins amount (idx+1)) in ch coins amount 0