let prime : int -> bool
= fun n ->
  let rec noDiv (m : int) : bool =
    m * m > n || (n mod m != 0 && noDiv (m+1) )
    in
    n >= 2 && noDiv 2;;

