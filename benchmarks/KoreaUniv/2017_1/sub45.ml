(* problem 1*) let rec fastexpt : int -> int -> int = fun b n -> if n = 0 then 1 else if n mod 2 = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2)) else b * (fastexpt b (n-1));;
(* problem 2*) let smallest_divisor : int -> int = fun n -> let rec mod_cal x = if n mod x = 0 then x else if x*x > n then n else mod_cal (x+1) in mod_cal 2;;
(* problem 3*) let iter : int * (int -> int) -> (int -> int) = fun (n,f) -> let rec appl n k = if n = 0 then fun x -> k(x) else appl (n-1) (fun x -> f(k(x))) in if n = 0 then fun x -> x+0 else appl (n-1) f;;
(* problem 4*) let rec product : (int -> int) -> int -> int -> int = fun f a b -> let func = fun x -> f(x) in if a < b then (func a) * (product f (a+1) b) else if a == b then func a else failwith "failure";;
(* problem 5*) let rec dfact : int -> int = fun n -> if (n mod 2 = 0) && (n > 2) then n * dfact (n-2) else if (n mod 2 = 1) && (n > 1) then n * dfact (n-2) else if n = 2 then 2 else if n = 1 then 1 else failwith "failure";;
(* problem 6*) let rec drop : 'a list -> int -> 'a list = fun l n -> match l with | [] -> [] | hd :: tl -> if n > 1 then drop tl (n-1) else if n = 1 then tl else failwith "failure";;
(* problem 7*) let unzip : ('a * 'b) list -> 'a list * 'b list = fin lst -> let lef (x,_) = x in let rig (_,x) = x in let newlst a b = (a,b) in let rec llst k = match k with | [] -> [] | hd :: tl -> (lef hd) :: (llst tl) in let rec rlst p = match p with | [] -> [] | hd :: tl -> (rig hd) :: (rlst tl) in newlst (llst lst) (rlst lst);;
(* problem 8*) let change : int list -> int -> int = fun coins amount ->
