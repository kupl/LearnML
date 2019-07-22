let rec max : int list -> int
= fun lst -> match lst with | []->0 |  hd::tl ->
let compare a b = if a>b then a else b in
let rec fold compare m n =
match m with | []->n | hd::tl -> compare hd (fold compare tl n) in
fold compare tl hd;;

 