let rec pascal : int * int -> int
= fun (n1, n2) -> match n1, n2 with (* TODO *)
| n1, 0 -> 1
| n1, n2 when n1=n2 -> 1 
| n1, n2 -> pascal(n1-1, n2-1) + pascal(n1-1, n2)
