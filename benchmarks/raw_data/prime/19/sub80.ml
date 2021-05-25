let prime : int -> bool
= fun n -> 
  let rec noDivisors (m) = 
    m*m>n || (n mod m !=0 && noDivisors (m+1))
  in
    n>=2 && noDivisors 2;;
    
prime 3;;
prime 6;;