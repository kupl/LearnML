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

