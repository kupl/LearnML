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

let sat : formula -> bool
= fun f ->
  let vars =
    let rec findval form vars =
      match form with
      |True -> []
      |False -> []
      |Var a -> if (let rec search mylist =
                    match mylist with
                    |[] -> false
                    |hd::tl -> if a = hd then true else search tl
                  in search vars ) then vars else a::vars
      |Neg a -> findval a vars
      |And (a,b) -> findval a (findval b vars)
      |Or (a,b) -> findval a (findval b vars)
      |Imply (a,b) -> findval a (findval b vars)
      |Iff (a,b) -> findval a (findval b vars)
    in findval f []
    in let rec check form l =
      match form with
      |True -> true
      |False -> false
      |Var a -> (let rec find mylist =
                  match mylist with
                  |hd::tl -> (match hd with
                            |(c,d) -> if c = a then d else find tl)
                  |[] -> false
                in find l)
      |Neg a -> not (check a l)
      |And (a,b) -> (check a l) && (check b l)
      |Or (a,b) -> (check a l) || (check b l)
      |Imply (a,b) -> (not (check a l)) || (check b l)
      |Iff (a,b) -> let myiff c d = (not c || d) && (c || not d) in myiff (check a l) (check b l)
    in let rec truth mylist cur = 
      match mylist with
      |hd::tl -> (truth tl ((hd, true )::cur)) || (truth tl ((hd, false)::cur))
      |[] -> check f cur 
    in truth vars [];;
