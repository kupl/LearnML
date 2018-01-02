let rec fold func l a =
match l with
|[] -> a
|hd::tl -> func hd (fold func tl a);;

let rec f : int list -> int
= fun lst -> fold (fun a b -> if (a > b) then a else b) lst 0;;

