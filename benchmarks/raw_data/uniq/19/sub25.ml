let rec reverse l =
  match l with
    | [] -> l
    | hd::tl -> reverse tl @ [hd]
;;

let rec insert a l =
  match l with
    | [] -> [a]
    | hd::tl -> if hd > a then a::hd::tl 
                else hd:: (insert a tl)
;;

let rec checker l1 a = 
  match l1 with
    | [] -> false
    | [k] -> if k = a then true else false
    | hd::tl -> if hd = a then true else checker tl a
;;

let rec finder l1 fin = 
  match l1 with
    | [] -> fin
    | hd::tl -> if checker fin hd = true then finder tl fin else finder tl (hd::fin)
;;

let uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    | [] -> []
    | _ -> reverse (finder lst [])
;;