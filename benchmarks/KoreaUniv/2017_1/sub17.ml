(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> (*TODO*)
  let rec impl _n = 
    if _n = 0 then 1
    else if _n mod 2 = 1 then b * impl (_n - 1)
    else (fun x -> x * x) (impl (_n / 2)) in
  impl n;;

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> (*TODO*)
  let rec iterator i =
    if n mod i = 0 then i
    else if i * i > n then n
    else iterator (i + 1) in
  iterator 2;;

(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)
  let rec impl _n =
    if _n = 0 then (fun x -> x)
    else (fun x -> f ((impl (_n - 1)) x)) in
  impl n;;

(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> (*TODO*)
  let rec impl n =
    if n == b then (f b)
    else (f n) * (impl (n+1)) in
  impl a;;

(* problem 5*)

let dfact : int -> int
= fun n -> (*TODO*)
  product (fun x -> x * 2 - (n mod 2)) 1 ((n + (n mod 2)) / 2);;

(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> (*TODO*)
  let rec impl _l _n =
    match _l with
    | [] -> []
    | hd::tl ->
    if _n = 0 then _l
    else impl tl (_n - 1) in
  impl l n;;

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*TODO*)
  let rec map f l =
    match l with
    | [] -> []
    | hd::tl -> (f hd)::(map f tl) in
  ((map (fun p -> (match p with (x, _) -> x)) lst), (map (fun p -> (match p with (_, x) -> x)) lst));;

(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)
  let rec impl l n =
    let rec loop m =
      match l with
      | [] ->
      if n = 0 then 1
      else 0
      | hd::tl ->
      if n < 0 || n < hd * m then 0
      else (impl tl (n - (hd * m))) + (loop (m + 1)) in
    loop 0 in
  impl coins amount;;
