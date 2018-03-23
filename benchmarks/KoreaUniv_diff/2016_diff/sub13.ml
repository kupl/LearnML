
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec map f l =
match l with
| [] -> []
| hd::tl -> (f hd)::(map f tl)

let rec tidy_up e =
match e with
| Const n -> Const n
| Var s -> Power (s, 1)
| Power (s, n) -> Power (s, n)
| Sum lst -> Sum (map tidy_up lst)
| Times lst -> Times (map tidy_up lst)

let rec m_a_p f s l =
match l with 
| [] -> []
| hd::tl -> f hd s :: m_a_p f s tl 

let rec isvar s e = match e with 
| Var s0 -> if (s <> s0) then false else true
| Power (s0, n) -> if (s<>s0) then false else if (n<1) then false else true
| _ -> false

let rec isconst s e = match e with
| Const n -> true
| Power (s0, n) -> if (s0 <> s) then true else if (n <> 0) then false else true
| _ -> false

let rec diff e s = 
let e0 = tidy_up e in
match e0 with
| Const n -> Const 0
| Var s0 -> diff (tidy_up (Var s0)) s
| Power (s0, n) -> if (s0 <> s) then (Const 0) else if (n > 0) then Times [Const n; Power (s, n-1)] else Const 0
| Sum lst -> Sum (m_a_p diff s lst)
(**)
| Times lst -> 
 let l0 = (List.find_all (isvar s) lst) in
 let l1 = (List.find_all (isconst s) lst) in
 begin match l0 with
 | [] -> Times []
 | hd::tl -> Times ( ((diff hd s) :: tl) @ l1 )
 end
