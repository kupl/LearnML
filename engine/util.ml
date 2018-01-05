let fst (a,b) = a

let snd (a,b) = b

let min : int list -> int
= fun lst ->
  let fst = List.hd lst in
  List.fold_left (fun min n -> if min>n then n else min) fst lst

let max : int list -> int
= fun lst ->
  let fst = List.hd lst in
  List.fold_left (fun max n -> if max<n then n else max) fst lst

let rec fix f x =
  let x' = f x in
    if x' = x then x' 
    else fix f x' 

let rec decreasing : int list -> bool
= fun lst ->
  match lst with
  | [] -> true
  | h::[] -> true
  | h1::h2::t -> if h1>=h2 then decreasing (h2::t) else false 

let (<<<) f g = fun x -> f (g x)
let (>>>) f g = fun x -> g (f x)

let id x = x
let flip f = fun y x -> f x y

let domof m = BatMap.foldi (fun k _ set -> BatSet.add k set) m BatSet.empty

(** This applies [List.fold_left], but the argument type is the same with
    [PSet.fold].  *)
let list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun f list init ->
  List.fold_left (flip f) init list

let list_map = List.map

let list_filter = List.filter

let list_split = List.split

let list_combine = List.combine

let list_hd = List.hd

let list_tl = List.tl

let list_fold2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f list1 list2 init ->
  let f' acc a b = f a b acc in
  List.fold_left2 f' init list1 list2

let list_rev : 'a list -> 'a list
= fun l ->
  let rec list_rev_rec l1 l2 =
    match l1 with
    | [] -> l2
    | a :: b -> list_rev_rec b (a :: l2) in
  list_rev_rec l []

let find_opt : 'a -> ('a, 'b) BatMap.t -> 'b option
= fun k m ->
  try Some (BatMap.find k m) with
  | Not_found -> None

let find_def : 'a -> ('a, 'b) BatMap.t -> 'b -> 'b
= fun k m default ->
  BatOption.default default (find_opt k m)

let link_by_sep sep s acc = if acc = "" then s else acc ^ sep ^ s

let string_of_list ?(first="[") ?(last="]") ?(sep=",") : ('a -> string)
  -> ('a list) -> string
= fun string_of_v list ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ list_fold add_string_of_v list "" ^ last

let string_of_array ?(first="{") ?(last="}") ?(sep=",") : ('a -> string)
  -> ('a list) -> string
= fun string_of_v list ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ list_fold add_string_of_v list "" ^ last

let string_of_set ?(first="{") ?(last="}") ?(sep=",") : ('a -> string)
  -> ('a BatSet.t) -> string
= fun string_of_v set ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ BatSet.fold add_string_of_v set "" ^ last

let string_of_map ?(first="{") ?(last="}") ?(sep=",\n") : ('a -> string)
  -> ('b -> string) -> (('a, 'b) BatMap.t) -> string
= fun string_of_k string_of_v map ->
  let add_string_of_k_v k v acc =
    let str = string_of_k k ^ " -> " ^ string_of_v v in
    link_by_sep sep str acc in
  if BatMap.is_empty map then "empty"
  else first ^ BatMap.foldi add_string_of_k_v map "" ^ last

let list2set l = list_fold BatSet.add l BatSet.empty
let set2list s = BatSet.fold (fun x l -> x::l) s []

let set_union_small_big small big = BatSet.fold BatSet.add small big

(* fixpoint operator for set *)
let rec fix_set : ('a BatSet.t -> 'a BatSet.t) -> 'a BatSet.t -> 'a BatSet.t 
= fun f init ->
  let next = f init in
    if BatSet.subset next init then init
    else fix_set f next
let rec lookup (x:'a) (l:('a * 'b) list) : 'b option =
  match l with
  | [] -> None
  | (y, v)::l -> if x = y then Some v else lookup x l

let rec update (m:('a * ('b list)) list) ((k, v):'a * 'b) : ('a * ('b list)) list =
  match m with
  | [] -> [(k, [v])]
  | (k', vs)::m -> if k = k' then (k', v::vs)::m else (k', vs)::update m (k, v)

let rec remove_first (f:'a -> bool) (l:'a list) : 'a list =
  match l with
  | [] -> []
  | x::l -> if f x then l else x::(remove_first f l)

(* Returns [0; ...; n-1] *)
let rec range (n:int) : int list =
  let rec f acc n =
    if n < 0 then acc else f (n :: acc) (n-1)
  in
  f [] (n-1)

(* Returns [1; ...; n] *)
let rec range1 (n:int) : int list =
  let rec f acc n =
    if n <= 0 then acc else f (n :: acc) (n-1)
  in
    f [] n

(* Returns [n; ...; m] *)
let rec rangen (n:int) (m:int) : int list =
  let rec f acc n =
    if n > m then acc else f (n :: acc) (n+1)
  in
    f [] n

let index_of (x:'a) (l:'a list) : int option =
  let rec helper n x l =
    match l with
    | []   -> None
    | y::l -> if x = y then Some n else helper (n+1) x l in
  helper 0 x l

let rec nub (l:'a list) : 'a list =
  match l with
  | [] -> []
  | x::l -> if List.mem x l then nub l else x :: nub l

let rec find (f:'a -> bool) (l:'a list) : 'a option =
  match l with
  | [] -> None
  | x::l -> if f x then Some x else find f l

let rec try_first (x:'a) (l:('a -> 'b option) list) : 'b option =
  match l with
  | [] -> None
  | f::l ->
    begin match f x with
    | Some y -> Some y
    | None -> try_first x l
    end

let rec try_first_lazy (l:'a option Lazy.t list) : 'a option =
  match l with
  | [] -> None
  | x::l ->
    begin match Lazy.force x with
    | Some v -> Some v
    | None -> try_first_lazy l
    end

let const (v:'a) : 'a -> 'a = fun _ -> v
let cons (x:'a) (l:'a list) : 'a list = x :: l

let rec replicate (n:int) (x:'a) : 'a list =
  if n <= 0 then [] else x::replicate (n-1) x

let rec find_first (f:'a -> bool) (l:'a list) : 'a option =
  match l with
  | [] -> None
  | x :: l -> if f x then Some x else find_first f l

let rec partitions (n:int) (k:int) : int list list =
  if n <= 0 || k <= 0 then
    []
  else if k == 1 then
    [[n]]
  else
    List.fold_left (fun res i ->
      List.append res @@ List.map (cons i) (partitions (n-i) (k-1)))
    [] (List.map ((+) 1) (range (n-k+1)))

type choice = MayNot | Must | May

let partitions_rel (k:int) : choice list list =
  let rec mark_part acc n c =
    if n >= k then acc else
    let ch =
      if c > 0 then May else if c = 0 then Must else MayNot
    in
      mark_part (ch :: acc) (n+1) (c-1)
  in
  List.map (mark_part [] 0) (range k |> List.rev)


let rec combinations (l:'a list list) : 'a list list =
  match l with
  | [] -> []
  | [x] -> List.map (fun n -> [n]) x
  | x :: l ->
    List.fold_left
      (fun res n -> List.append res (List.map (cons n) (combinations l)))
      [] x

let rec disjoint (l1:'a list) (l2:'a list) : bool =
  match l1 with
  | [] -> true
  | x::l1 -> if List.mem x l2 then false else disjoint l1 l2

let rec is_some : 'a option -> bool =
  function | Some v -> true | None -> false

type ('a, 'b) either = Left of 'a | Right of 'b

let rec all_eq (p:'a -> 'a -> bool) (l:'a list) : bool =
  match l with
  | [] -> true
  | [x] -> true
  | x::y::l -> if not (p x y) then false else all_eq p (y::l)

let rec partition (n:int) (l:'a list) : 'a list * 'a * 'a list =
  let rec search pre n post =
    if n = 0 then
      (pre, List.hd post, List.tl post)
    else
      search (List.hd post :: pre) (n-1) (List.tl post)
  in
    if n < List.length l then
      search [] n l
    else
      raise @@ Invalid_argument "(partition) index out of range"

let rec separate ~f:(f:'a -> bool) (l:'a list) : 'a list * 'a list =
  let rec sep acc l =
    match l with
    | [] -> acc
    | x :: l ->
        let (lacc, racc) = acc in
        if f x then sep (lacc @ [x], racc) l else sep (lacc, racc @ [x]) l
  in
    sep ([], []) l

let rec time_action ~f:(f: unit -> 'a) : float * 'a =
  let t1  = Unix.gettimeofday () in
  let res = f () in
  let t2  = Unix.gettimeofday () in
  (t2 -. t1, res)
