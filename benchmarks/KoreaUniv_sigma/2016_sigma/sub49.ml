let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> match b-a with
| 0 -> f(a)
| _ -> f(a) + sigma f (a+1) b
