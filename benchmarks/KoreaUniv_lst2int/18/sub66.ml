let rec length_of_list : int list -> int
= fun lst ->
    match lst with
      | [] -> 0 
      | hd::tl -> length_of_list tl + 1;;


let rec pow : int -> int -> int
= fun a b ->
  if b = 0 then 1 else (pow a (b-1)) * a;; 


let lst2int : int list -> int
= fun lst ->
  let a = 0 in
    let rec adding_list : int list -> int -> int
    = fun lst idx ->
      match lst with
        | [] -> 0
        | hd::tl -> 
          adding_list tl (a+1) + hd * (pow 10 (length_of_list lst - a -1))
    in adding_list lst a;;