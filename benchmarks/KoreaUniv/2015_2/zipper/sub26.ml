let rec zipper : int list * int list -> int list
=fun (list1,list2) -> 
match list1 with
    | [] -> list2
    | head1::tail1 -> (match list2 with
        | [] -> list1
        | head2::tail2 -> head1::head2::(zipper (tail1,tail2)));;
