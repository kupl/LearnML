(* problem 1 *)
let rec fastexpt
= fun b n ->
    match n with
    | 0 -> 1
    | _ -> if (n mod 2 = 1) then b*(fastexpt b (n-1)) else (fastexpt b (n/2))*(fastexpt b (n/2))

(* problem 2 *) 
let rec devisor
= fun n d -> match d with
        | _ -> if (n mod d = 0) then d else (devisor n (d+2))
let smallest_divisor
= fun n ->
    match n with
    | _ -> if (n mod 2 = 0) then 2 else (devisor n 3)

(* problem 3 *) 
let rec compose (f : int->int) (g : int->int) (x : int) = f(g x)
let rec all_compose n (f: int-> int) = if (n = 1) then f else compose f (all_compose (n-1) f);;
let rec iter : int *(int-> int) -> (int -> int)
= fun(n,f) -> match n with
          | 0 -> fun x -> x 
          | _ -> all_compose n f

(* problem 4 *)
let rec product 
= fun f a b -> match b with
          | _ -> if (b = a) then (f a) else (f b)*(product f a (b-1)) 

(* problem 5 *)
let rec dfact
= fun n -> match n with
       | 1 -> 1
       | 2 -> 2
       | _ -> if (n mod 2 = 0) then (fastexpt 2 (n/2))*(product (fun x -> x) 2 (n/2))
              else (product (fun x-> x) 1 n)/(dfact (n-1))

(* problem 6 *) 
let rec drop
= fun l n -> match l with
      | [] -> []
      | hd::tl -> if (n = 1) then tl else (drop tl (n-1)) 

(* problem 7 *)
let rec decompose_x (x, _) = x::[]
let rec decompose_y (_, y) = y::[]
let rec decompose_xst = fun l -> match l with
        | [] -> []
        | hd::tl -> (decompose_x hd)@(decompose_xst tl)
let rec decompose_yst = fun l -> match l with 
        | [] -> []
        | hd::tl -> (decompose_y hd)@(decompose_yst tl)
let rec unzip 
= fun lst -> (decompose_xst lst, decompose_yst lst) 

(*problem 8 *)
let rec nth l n = match l with
        | [] -> 0
        | hd::tl -> if (n = 0) then hd else (nth tl (n-1))
let rec length l = match l with
        | [] -> 0
        | hd::tl -> 1 + (length tl)
let rec count (l : int list) (m : int) (n : int) =
        if (n = 0) then 1
        else if (n < 0) then 0
        else if (m<=0)&&(n>0) then 0
        else (count l (m-1) n) + (count l m (n-(nth l (m-1))))
let rec change : int list -> int -> int
= fun coins amount -> match amount with
        | _ -> count coins (length coins) amount

