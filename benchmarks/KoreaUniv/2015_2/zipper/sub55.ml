let rec zipper : int list * int list -> int list
= fun (a,b) -> match a,b with
| [],[] -> []
| ahd::atl,bhd::btl -> ahd::bhd::(zipper (atl, btl))
| [], b -> b
| a, [] -> a
