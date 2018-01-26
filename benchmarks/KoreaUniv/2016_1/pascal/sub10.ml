let rec pascal : int * int -> int
= fun (n1, n2) -> if n1 = 0 && n2 = 0 then 1 else
        if n1 < 0 || n2 < 0 then 0 else
                pascal (n1 - 1, n2) + pascal (n1 - 1, n2 - 1);;
