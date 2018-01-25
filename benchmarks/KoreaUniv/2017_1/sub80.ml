(* problem 1*)


 let rec fastexpt b n = 
  if n = 0 then 1
  else if n / 2 =0 then (fastexpt b n / 2) * (fastexpt b n / 2)
  else b * (fastexpt b n-1);;



(* problem 2*)

let smallest_divisor : int -> int
= fun n -> for i=2 to sqrt n do
  if n mod i = 0 then i;; 



(* problem 3*)


let compose f g = fun x -> f(g(x));;
let iter : int * (int -> int) -> (int -> int)
= rec fun (n,f) -> 
  if n = 0 then 1
  else compose compose fun ((fun n-1) f)



(* problem 4*)


let rec product a b =
if a = b then a
else (product 1 b) / (product 1 a-1);; 



(* problem 5*)


  let rec dfact n = 
  if n = 1 then 1
  else if n = 0 then 1
  else n * (dfact n-2);; 



(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> 
  match l with
  | [] -> []
  | hd::tl -> (hd+n)::tl;;
  



(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun ('a * 'b) lst -> 
  match lst with
  | [] -> []
  | hd::tl -> fun ('a * 'b) tl
  | length[lst]=1 -> ([a],[b]);; 



(* problem 8*)


  let rec remove a l =
  match l with
  | [] -> []
  | hd::tl -> if hd = a then tl else remove a tl;;
  
  let n = 0;;

  let change : int list -> int -> int
= fun coins amount -> 
  match coins with
  | [] -> 0 
  | hd::tl -> if amount mod hd <> 0 then fun (remove hd coins) (amount-hd*amount mod hd) 
  else n = n+1 in while(amount / hd <>0 ) do fun (remove hd coins) (amount-hd*amount mod hd) done
  n;; 

  