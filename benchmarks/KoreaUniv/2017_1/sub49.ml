(* problem 1*)

let rec fastexpt : int -> int -> int = fun b n ->
  match n with
  | 0 -> 1
  | _ -> if n mod 2 = 0
    then let n2 = fastexpt b (n / 2) in n2*n2
    else b * fastexpt b (n - 1);;

(* problem 2*)

let smallest_divisior : int -> int = fun n ->
  let rec div : int -> int -> int = fun n r ->
    if r*r > n then n else
      if n mod r = 0 then r
      else div n (r + 1)
  in div n 2;;

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int) = fun (n, f) ->
  match n with
  | 0 -> fun x -> x
  | _ -> fun x -> f ( iter (n - 1, f) x);;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int = fun f a b ->
  if a > b then 0 else
    if a = b then f a
    else ( f a ) * ( product f (a + 1) b );;

(* problem 5*)

let rec dfact : int -> int = fun n ->
  match n with
  | 0 | 1 -> 1
  | _ -> n * dfact (n - 2);;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list = fun l n ->
  match l with
  | [] -> []
  | hd::tl -> match n with
    | 0 -> l
    | _ -> drop tl (n - 1);;

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list = fun lst ->
  let fst (x , _) = x in let snd (_ , x) =x in
  match lst with
  | [] -> ([], [])
  | hd::tl -> 
    let l = unzip tl in
    ((fst hd)::(fst l), (snd hd)::(snd l));;

(* problem 8*)

let rec change : int list -> int -> int = fun coins amount -> 
  match amount with
  | 0 -> 1
  | _ -> match coins with
    | [] -> 0
    | hd::tl -> (change tl amount) + if amount < hd then 0 else change coins (amount -hd);;
