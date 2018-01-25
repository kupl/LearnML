let rec fastexpt : int -> int -> int
= fun b n -> 
  if n = 0 then 1 
  else if n = 1 then b
  else if n mod 2 = 0 then fastexpt b (n / 2) * fastexpt b (n / 2)
  else b * fastexpt b (n - 1);;

let smallest_divisor : int -> int
= fun n -> 
  let p = 2 in
    let rec check p = 
      if p * p > n then n
      else if n mod p = 0 then p
      else check (p + 1)
    in
    if p < 2 then 1
    else check 2;;

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
  if n = 0 then fun x -> x
  else fun x -> f ((iter (n - 1, f)) x);;

let product : (int -> int) -> int -> int -> int
= fun f a b -> 
  if a > b then 1
  else if a = b then f a
  else 
    let rec innerProd 
    = fun f n b c ->
      if n > b then c
      else if n = b then c * f n
      else innerProd f (n + 1) b (c * f n) in 
      
      innerProd f a b 1;;

let dfact : int -> int
= fun n ->
  let innerK = fun x ->
    if n mod 2 = 0 then 2 * x
    else 2 * x - 1 in
  if n mod 2 = 0 then product innerK 1 (n / 2)
  else product innerK 1 ((n + 1) / 2);;

let rec drop : 'a list -> int -> 'a list
= fun l n ->
  if n = 0 then l
  else match l with
    | [] -> []
    | h :: t -> drop t (n - 1) 

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
  match lst with 
  | [] -> ([],[])
  | h :: t -> 
  let rec  makeList : 'a * 'b -> ('a *'b) list -> 'a list * 'b list -> 'a list * 'b list
  = fun (a,b) lst (alst,blst) ->
    match lst with
    | [] -> (alst @ [a],blst @ [b])
    | h :: t -> makeList h t (alst @ [a], blst @ [b])
  in makeList h t ([],[]);;

let rec change : int list -> int -> int
= fun coins amount -> 
  if amount = 0 then 1
  else if amount < 0 then 0
  else match coins with
  | [] -> 0
  | h::t -> (change coins (amount - h)) + (change t amount)

let plustwo = fun x -> x + 2;;
let pointself : int -> int
= fun x -> x;;
