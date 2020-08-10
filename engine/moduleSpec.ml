(*********************************)
(* For process external functions*)
(*********************************)
exception ListError

exception Failure of string
exception Invalid_argument of string
exception Exit

let assert' : bool -> 'a
= fun b -> raise (Failure ("Assert failure"))

let invalid_arg : string -> 'a
= fun s -> raise (Invalid_argument s)

let failwith : string -> 'a
= fun s -> raise (Failure s)

let __list_hd__ : 'a list -> 'a
=fun lst -> 
  match lst with
  |[] -> raise ListError
  |hd::tl -> hd

let __list_tl__ : 'a list -> 'a list
=fun lst -> 
  match lst with
  |[] -> raise ListError
  |hd::tl -> tl

let rec __list_map__ : ('a -> 'b) -> 'a list -> 'b list
=fun func lst -> 
  match lst with
  |[] -> []
  |hd::tl -> (func hd) :: (__list_map__ func tl)

let rec __list_mem__ : 'a -> 'a list -> bool
=fun elem lst -> 
  match lst with
  |[] -> false
  |hd::tl -> (hd=elem) || (__list_mem__ elem tl)

let rec __list_exists__ : ('a -> bool) -> 'a list -> bool
=fun pred lst -> 
  match lst with
  |[] -> false
  |hd::tl -> (pred hd) || (__list_exists__ pred tl)

let rec __list_filter__ : ('a -> bool) -> 'a list -> 'a list
=fun pred lst ->
  match lst with
  |[] -> []
  |hd::tl -> if(pred hd) then hd :: (__list_filter__ pred tl) else __list_filter__ pred tl

let __list_append__ : 'a list -> 'a list -> 'a list
=fun lst1 lst2 -> lst1 @ lst2

let rec __list_length__ : 'a list -> int
=fun lst -> 
  match lst with
  |[] -> 0
  |_::tl -> 1 + __list_length__ tl

let rec __list_nth__ : 'a list -> int -> 'a
=fun lst n -> 
  match lst with
  |[] -> raise ListError
  |hd::tl -> if(n=0) then hd else __list_nth__ tl (n-1)

let rec __list_rev__ : 'a list -> 'a list
=fun lst ->
  let rec rev_acc acc lst = 
    match lst with
    |[] -> acc
    |hd::tl -> rev_acc (hd::acc) tl
  in rev_acc [] lst

let rec __list_foldl__ : ('a -> 'b  -> 'a) -> 'a -> 'b list -> 'a
=fun func acc lst ->
  match lst with
  |[] -> acc
  |hd::tl -> __list_foldl__ func (func acc hd) tl

let rec __list_foldr__ : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
=fun func lst acc ->
  match lst with
  |[] -> acc
  |hd::tl -> func hd (__list_foldr__ func tl acc)

let rec __list_rev_map__ : ('a -> 'b) -> 'a list -> 'b list
=fun func lst ->
  match lst with
  |[] -> []
  |hd::tl -> __list_foldl__ (fun acc elem -> (func elem)::acc) [] lst

let rec __list_sort__ : ('a -> 'a -> int) -> 'a list -> 'a list
=fun ord lst -> 
  let rec insertion lst elem =
    match lst with
    |[] -> [elem]
    |hd::tl -> if (ord elem hd) > 0 then hd::(insertion tl elem) else elem::lst
  in __list_foldl__ insertion [] lst

let rec __list_memq__ : 'a -> 'a list -> bool
=fun elem lst ->
  match lst with
  |[] -> false
  |hd::tl -> (hd==elem) || (__list_mem__ elem tl)

let rec __list_rev_append__ : 'a list -> 'a list -> 'a list
= fun lst1 lst2 -> 
  match lst1 with
  |[] -> lst2
  |hd::tl -> __list_rev_append__ tl (hd::lst2)

let __list_map_i__ : (int -> 'a -> 'b) -> 'a list -> 'b list
= fun func lst ->
  let rec map_with_counter : (int -> 'a -> 'b) -> int -> 'a list -> 'b list 
  = fun func count lst ->
    match lst with
    | [] -> lst
    | hd::tl -> (func count hd)::(map_with_counter func (count+1) tl)
  in 
  map_with_counter func 0 lst 

let rec __list_for_all__ : ('a -> bool) -> 'a list -> bool
= fun pred lst ->
  match lst with
  |[] ->  true
  |hd::tl -> (pred hd)&&(__list_for_all__ pred tl)

let rec __list_find__ : ('a -> bool) -> 'a list -> 'a
= fun pred lst ->
  match lst with
  |[] -> raise ListError
  |hd::tl -> if(pred hd) then hd else __list_find__ pred tl

let rec __list_assoc__ : 'a -> ('a * 'b) list -> 'b
= fun idx l ->
  match l with
  | [] -> raise (Failure "Not_found")
  | (a, b)::tl -> if a=idx then b else __list_assoc__ idx tl

let rec __string_concat__ : string -> string list -> string
= fun sep sl ->
  match sl with
  | [] -> ""
  | hd::tl -> hd ^ sep ^ (__string_concat__ sep tl)

let rec string_of_int : int -> string
= fun n ->
  if n < 0 then "-" ^ string_of_int (-n)
  else match n with
  | 0 -> "0" | 1 -> "1" | 2 -> "2"
  | 3 -> "3" | 4 -> "4" | 5 -> "5"
  | 6 -> "6" | 7 -> "7" | 8 -> "8"
  | 9 -> "9" | _ -> (string_of_int (n / 10)) ^ (string_of_int (n mod 10))
 
let max_int : int = 4611686018427387903

let min_int : int = -4611686018427387903

let fst (t,_) = t

let snd (_,t) = t

(*let max a b = if(a>b) then a else b*)

let min a b = if(a>b) then b else a

let compare a b = if(a=b) then 0 else if (a>b) then 1 else -1
