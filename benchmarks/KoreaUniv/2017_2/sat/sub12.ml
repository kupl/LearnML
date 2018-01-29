(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec find_all_string : formula -> string list -> string list
= fun f l-> match f with
           | True -> l
           | False -> l
           | Var s -> s::l
           | Neg f1 -> find_all_string f1 l
           | And (f1,f2) -> find_all_string f1 (find_all_string f2 l)
           | Or (f1,f2) -> find_all_string f1 (find_all_string f2 l)
           | Imply (f1,f2) ->find_all_string f1 (find_all_string f2 l)
           | Iff (f1, f2) -> find_all_string f1(find_all_string f2 l)

let rec true_in_string : formula->string->formula
= fun f s -> match f with
            | True -> True
            | False -> False
            | Var t -> if t=s then True else Var t
            | Neg f1 -> Neg (true_in_string f1 s)
            | And (f1, f2)-> And (true_in_string f1 s, true_in_string f2 s)
            | Or (f1, f2) -> Or (true_in_string f1 s, true_in_string f2 s)
            | Imply (f1, f2) -> Imply(true_in_string f1 s, true_in_string f2 s)
            | Iff (f1, f2) -> Iff(true_in_string f1 s, true_in_string f2 s)

let rec false_in_string : formula->string->formula
= fun f s -> match f with
            | True -> True
            | False -> False
            | Var t -> if t=s then False else Var t
            | Neg f1 -> Neg (false_in_string f1 s)
            | And (f1, f2) -> And (false_in_string f1 s, false_in_string f2 s)
            | Or(f1, f2) -> Or(false_in_string f1 s, false_in_string f2 s)
            | Imply(f1, f2) -> Imply(false_in_string f1 s, false_in_string f2 s)
            | Iff(f1, f2) -> Iff(false_in_string f1 s, false_in_string f2 s)

let rec sum_list : formula list -> formula list -> formula list
=fun fl1 fl2 -> match fl1 with
                |hd::tl -> hd::(sum_list tl fl2)
                |_ -> fl2

let rec kwang : formula->string list ->formula list
= fun f sl -> match sl with
                |hd::tl -> sum_list (kwang (true_in_string f hd) tl) (kwang (false_in_string f hd) tl)
                |_ -> [f]

let rec check_sat : formula -> int
= fun f -> match f with
           |True -> 1
           |False -> 0
           |Neg f1 -> if (check_sat f1 = 1) then 0 else 1
           |And(f1,f2) -> (check_sat f1)*(check_sat f2)
           |Or(f1,f2) -> if ((check_sat f1)+(check_sat f2)=0) then 0 else 1
           |Imply(f1,f2) -> if ((check_sat f1)=1 & (check_sat f2)=0) then 0 else 1
           |Iff(f1,f2) -> if((check_sat f1)=(check_sat f2)) then 1 else 0
let rec check_list_sat : formula list -> int
=fun fl -> match fl with
           | hd::tl -> (check_sat hd)+(check_list_sat tl)
           | _ -> 0

let rec sat : formula -> bool
= fun f -> if ( check_list_sat (kwang f  (find_all_string f []))=0) then false else true

