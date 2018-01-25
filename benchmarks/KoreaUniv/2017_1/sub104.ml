(* problem 1*)

let check_even n = if (n/2)+(n/2)=n then true else false

let rec fastexpt : int -> int -> int
= fun b n -> match n with
            | 1 -> b
            | 2 -> b*b
            | _ -> if check_even n then fastexpt (fastexpt b (n/2)) 2 else b*(fastexpt b (n-1))

(* problem 2*)

let smaller_root a n = if (n*n)<a then true else if (n*n)=a then true else false
let check_div a n = if (a/n)*n=a then true else false

let rec divisor a n = if smaller_root a n then (if check_div a n then n else divisor a (n+1)) else a
  
let smallest_divisor : int -> int
= fun n -> divisor n 2

(* problem 3*)
let fun_add f1 f2 = fun x -> f2(f1 x)
let rec fun_adds n f = if n=0 then fun x->x else fun_add f (fun_adds (n-1) f) 

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> fun_adds n f

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a=b then f a else (f a)*product f (a+1) b

(* problem 5*)

let rec dfact : int -> int
= fun n -> match n with
 | 1 -> 1
 | 2 -> 2
 | _ -> (product (fun x->x) 1 n) / dfact (n-1)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> if n=0 then l
else
  match l with
  | [] -> []
  | hd::tl-> drop tl (n-1) 
(* problem 7*)

let first (x,y) = x
let second (x,y) = y

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
  | [] -> ([],[])
  | hd::tl -> (first hd::first (unzip tl), second hd::second (unzip tl))

(* problem 8*)
let rec insert a l = match l with
  | [] -> [a]
  | hd::tl -> if hd>a then a::l else hd::(insert a tl)
let rec sort l =
  match l with
  | [] -> []
  | hd::tl -> insert hd (sort tl)

let rec ele_div c a = match c with
  | [] -> 0
  | hd::tl -> if a=0 then 1 else if a>hd then (ele_div tl a) + ele_div c (a-hd) else if a=hd then 1 else if a>0 then ele_div tl (a-hd) else 0

let change : int list -> int -> int
= fun coins amount -> match coins with
                      | [] -> 0
                      | hd::tl-> ele_div (sort coins) amount

