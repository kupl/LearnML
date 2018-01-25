(* problem 1*)
  let rec  fastexpt : int -> int -> int
  = fun b n -> if n=0  then 1 else
  if (n mod 2) = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2)) else
  b * (fastexpt b (n-1));;

(* problem 2*)
  let smallest_divisor : int -> int
  = fun n -> let d = 2 in
  let rec divisor : int -> int -> int
  = fun d n -> if (int_of_float (sqrt (float_of_int n))) >= d then
  if (n mod d) = 0 then d else divisor (d+1) n else n in
  divisor d n;;

(* problem 3*)
  let iter : int * (int -> int) -> (int -> int)
  = fun (n,f) ->
  let rec compose : int * (int -> int) * (int -> int) -> int -> int
  = fun (n,f,g) -> if n=0 then fun x -> g(x)
  else compose(n-1, f, fun x-> g(f(x))) in
  compose(n, f, fun x -> x);;

(* problem 4*)
  let rec product : (int -> int) -> int -> int -> int
  = fun f a b -> if a=b then f(a) 
  else f(a) * (product f (a+1) b);;

(* problem 5*)
  let dfact : int -> int
  = fun n -> if (n mod 2) =0 then product (fun x -> 2*x) 1 (n/2)
  else product (fun x -> 2*x-1) 1 ((n+1)/2);;

(* problem 6*)
  let rec drop : 'a list -> int -> 'a list
  = fun l n -> if n=0 then l else
  drop (match l with | [] -> [] | hd::tl -> tl) (n-1);;

(* problem 7*)
  let unzip : ('a * 'b) list -> 'a list * 'b list
  = fun lst -> let rec flist : ('a * 'b) list -> 'a list 
  = fun flst -> match flst with
  |[] -> []
  | hd::tl -> (match hd with
      | (x,_) -> x::(flist tl)) in
  let rec slist : ('a * 'b) list -> 'b list
  = fun slst -> match slst with
  |[] -> []
  | hd::tl -> (match hd with
      | (_,x) -> x::(slist tl)) in
  (flist lst, slist lst);;

(* problem 8*)
 (*  let rec insert : int list -> int -> int list
  = fun lst n ->  match lst with
  | [] -> [n]
  | hd::tl -> (if hd>n then hd::(insert tl n) else n::lst);;

  let rec sort : int list -> int list
  = fun lst ->  match lst with
  | [] -> []
  | hd::tl -> insert (sort tl) hd;;
  let rec sol : int list -> int -> int -> int
  = fun coins amount n -> match coins with
  | [] -> 0
  | hd::tl -> if n < 0 then 0
  else if (amount-(n*hd))=0 then 1+(sol coins amount (n-1))
  else if (amount-(n*hd))>0 then 0+((sol coins amount (n-1)) + (sol tl (amount - n*hd) ((amount - n*hd)/(hdcut tl)) ))
  else sol coins amount (n-1);;
  let sortedchange : int list -> int -> int
  = fun coins amount -> match coins with 
  | [] -> 0
  | hd::tl -> sol coins amount (amount/hd);; 
  let change : int list -> int -> int
  = fun coins amount -> sortedchange (sort coins) amount ;;
  *)
  let change : int list -> int -> int
  = fun coins amount -> 
  let hdcut : int list -> int
  = fun lst -> match lst with
  | [] -> 1
  | hd::tl -> hd in
  let rec insert : int list -> int -> int list
  = fun lst n -> match lst with
  | [] -> [n]
  | hd::tl -> (if hd>n then hd::(insert tl n) else n::lst) in 
  let rec sort : int list -> int list
  = fun lst -> match lst with
  | [] -> []
  | hd::tl -> insert (sort tl) hd in
  let rec sol : int list -> int -> int -> int
  = fun coins amount n -> match coins with
  | [] -> 0
  | hd::tl -> if n < 0 then 0
  else if (amount-(n*hd))=0 then 1+(sol coins amount (n-1))
  else if (amount-(n*hd))>0 then 0+((sol coins amount (n-1)) + (sol tl (amount - n*hd) ((amount - n*hd)/(hdcut tl)) ))
  else sol coins amount (n-1) in
  let sortedchange : int list -> int -> int
  = fun coins amount -> match coins with
  | [] -> 0
  | hd::tl -> sol coins amount (amount/hd) in
  sortedchange (sort coins) amount;;
