let lst2int : int list -> int
= fun a->
let rec lst2int_sub : int -> int list -> int = fun a b ->
match b with
|hd::tl -> lst2int_sub (a*10+hd) tl
|_ -> a
in lst2int_sub 0 a;;