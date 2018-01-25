(*Problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if (n mod 2=0) then (fastexpt b (n/2)) * (fastexpt b (n/2))
  else (if (n=1) then b else b*(fastexpt b (n-1)))

(*Problem 2*)
let checksqrt a n = a > sqrt(n)

let rec find_sd a n = if (checksqrt (float_of_int a) (float_of_int n)) then n
  else (if (n mod a = 0) then a else find_sd (a+2) n)

let smallest_divisor : int -> int
= fun n -> if (n mod 2 = 0) then 2 else
  (
    let a = 3 in
    find_sd a n
  )

(*Problem 3*)
let comp f g = fun x -> f(g x)

let rec loop  : int * (int -> int) * (int -> int) -> (int -> int)
= fun (n,f,g) -> if(n=1) then g
  else loop((n-1),f,(comp f g))

let iden_func n = n

let error n = (-1) 

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if (n<0) then error 
  else(
      if (n=0) then iden_func
      else loop (n,f, f)
      )

(*Problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->  if (a<b) then (f(a)*(product f (a+1) b))
  else f(b)

(*Problem 5*)
let dfact : int -> int
= fun n -> if(n mod 2 =0)
  then (product (fun x -> 2*x) 1 (n/2))
  else (product (fun x -> (2*x-1)) 1 ((n+1)/2))

(*Problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> if(n <= 0) then l
  else drop (match l with
      | [] -> []
      | hd::tl -> tl) (n-1)

(*Problem 7*)
let fst (x,_) = x
let snd (_,x) = x

let rec tuple1 lst=
match lst with
|[] -> []
|hd::tl -> if(tl=[]) then fst hd::[] else (fst hd)::(tuple1 tl)

let rec tuple2 lst=
match lst with
|[] -> []
|hd::tl -> if(tl=[]) then snd hd::[] else (snd hd)::(tuple2 tl)


let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (tuple1 lst, tuple2 lst)


(*Problem 8*)
let coincost l=
  match l with
  |[]->0
  |hd::tl -> hd

let coinnext l =
  match l with
  |[]->[]
  |hd::tl -> tl

let rec change : int list -> int -> int
= fun coins amount -> if(amount = 0) then 1
  else if(amount < 0) then 0
  else if(coins=[]) then 0
  else (change coins (amount-(coincost coins))) + (change (coinnext coins) amount)



