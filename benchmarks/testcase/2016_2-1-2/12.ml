let rec fold func l a =
  match l with
    | [] -> a
    | hd::tl -> func hd (fold func tl a);;

let rec f : int list -> int
  = fun lst -> 
    let small a b =
      if a > b then b
      else a in fold (small)lst 99999;;
