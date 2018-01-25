(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
  let square x = x*x in
  if (n <= 1) then b
  else if (n mod 2 = 0) then square(fastexpt b (n/2))
  else b*(fastexpt b (n-1))
;;

(* problem 2*)
let smallest_divisor : int -> int
= fun n ->
  let rec it = fun c ->
    if (c*c > n) then n
    else if (n mod c = 0) then c
    else it(c+1)
  in
  it 2
;;

(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
  if (n <= 1) then f
  else fun x -> f((iter((n-1),f)) x) 
;;

(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
  if (a > b) then 1
  else (f a)*(product f (a+1) b)
;;

(* problem 5*)
let rec dfact : int -> int
= fun n ->
  product (fun x->(2*x - n mod 2)) 1 ((n + (n mod 2))/2)
;;

(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n ->
  if (n <= 0) then l
  else match l with
  | [] -> []
  | hd::tl -> drop tl (n-1)
;;

(* problem 7*)
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
  let rec iterate = fun l al bl ->
    match l with
    | [] -> (al,bl)
    | hd::tl -> iterate tl (al@[fst hd]) (bl@[snd hd])
  in
  iterate lst [] []
;;

(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount ->
  if (amount < 0) then 0 else
  let rec iterate : int -> int -> int -> int -> int list -> int
  = fun coin amount sum idx coins ->
    if idx = 0 then (sum + change coins amount)
    else iterate coin amount (sum + change coins (amount-(idx*coin))) (idx-1) coins
  in
  match coins with
  | [] -> if amount = 0 then 1 else 0
  | hd::tl -> iterate hd amount 0 (amount/hd) tl
;;