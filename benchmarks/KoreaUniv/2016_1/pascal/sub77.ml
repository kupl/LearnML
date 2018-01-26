let rec pascal : int * int -> int
= fun (n1, n2) ->
    let rec loop n r c =
        if n2 >= r
            then loop (n - 1) (r + 1) (c * n / r)
            else c in
    loop n1 1 1 (* because pascal is combination *)
