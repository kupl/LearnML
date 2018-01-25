(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
  match lst with
  | [] -> []
  | hd :: tl -> if pred hd then hd :: filter pred tl
                           else filter pred tl

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
              | [] -> b
              | hda :: tla -> (match b with
                            | [] -> a
                            | hdb :: tlb -> [hda ; hdb] @ (zipper (tla , tlb)) )

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
  if n < 0 then raise (Failure ("first parameter should not be negative"))
           else match n with
                | 0 -> (fun x -> x)
                | _ -> (fun x -> iter( (n - 1), f) (f x))

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
  match aexp with
  | Const n -> Const 0
  | Var st -> if st = x then Const 1 else Const 0
  | Power (st, n) -> if st = x then Times [Const n; Power (st , (n - 1))]
                               else Const 0
  | Times [] -> Const 0
  | Times (hd :: []) -> Times [ diff (hd , x) ; Const 1]
  | Times (hd :: tl) -> let hdd = diff (hd , x)         in
                        let ttl = Times tl              in
                        let hdp = Times [hdd ; ttl]     in
                        let tlp = Times [hd; diff (ttl , x)]  in
                        Sum [hdp ; tlp]
  | Sum [] -> Const 0
  | Sum (hd :: []) -> Sum [(diff (hd , x)) ; Const 0]
  | Sum (hd :: tl) -> Sum [(diff (hd , x)) ; diff (Sum tl, x)]

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let calculator : exp -> int
= fun e ->
    let rec xcal : exp -> (int option -> int)
    = fun ee ->
        match ee with
        | X -> (fun q -> match q with
                        | Some a -> a
                        | None -> raise (Failure " no argument for X " ))
        | INT n -> (fun q -> n)
        | ADD (a , b) -> (fun q -> xcal_sub a b (+) q)
        | SUB (a , b) -> (fun q -> xcal_sub a b (-) q)
        | MUL (a , b) -> (fun q -> xcal_sub a b ( * ) q)
        | DIV (a , b) -> (fun q -> xcal_sub a b (/) q)
        | SIGMA (a , b , c) -> (fun q -> let a' = xcal a q in
                                     let b' = xcal b q in
                                     sigma_sub a' b' c)
    and sigma_sub : int -> int -> exp -> int
    = fun n m c ->
        let c' = xcal c in
        if n > m
            then raise (Failure "Inappropriate sigma-inputs")
            else if n = m
                    then c' (Some n)
                    else (c' (Some n)) + (sigma_sub (n + 1) m c)
    and xcal_sub : exp -> exp -> (int -> int -> int) -> (int option -> int)
    = fun a b bop -> let a' = xcal a in
                     let b' = xcal b in
                     (fun q -> bop (a' q) (b' q)) in
    xcal e None


        (*
    let rec xcal : exp -> (int -> int)
    = fun ee ->
        match ee with
        | X -> (fun q -> q)
        | INT n -> (fun q -> n)
        | ADD (a , b) -> (fun q -> xcal_sub a b (+) q)
        | SUB (a , b) -> (fun q -> xcal_sub a b (-) q)
        | MUL (a , b) -> (fun q -> xcal_sub a b ( * ) q)
        | DIV (a , b) -> (fun q -> xcal_sub a b (/) q)
        | SIGMA (a , b , c) -> (fun q -> let a' = xcal a q in
                                         let b' = xcal b q in
                                         sigma_sub a' b' c)
    and sigma_sub : int -> int -> exp -> int
    = fun n m c ->
        let c' = xcal c in
        if n > m
           then raise (Failure "Inappropriate sigma-inputs")
           else if n = m
                   then c' n
                   else (c' n) + (sigma_sub (n + 1) m c)
    and xcal_sub : exp -> exp -> (int -> int -> int) -> (int -> int)
    = fun a b bop ->
        let a' = xcal a in
        let b' = xcal b in
        (fun q -> bop (a' q) (b' q))
    in
    xcal e 0
    *)
