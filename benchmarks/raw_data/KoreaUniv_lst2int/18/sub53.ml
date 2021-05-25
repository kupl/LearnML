let rev list =
    let rec exc lts = function
      | [] -> lts
      | h::t -> exc (h::lts) t in
    exc [] list;;
let  lst2int : int list -> int
= fun lst -> (*TODO*)
    let rec sum = fun l->
    match l with 
      |[]->0
      |hd::tl->hd+10*(sum tl) in sum(rev lst);;
      
lst2int [2;3;4;5];;