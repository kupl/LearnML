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
