let rec check : int*int -> bool
= fun (n,m) -> if m=1 then true
              else 
                if n mod m=0 then false
                else check(n,m-1);;
  
let prime : int -> bool
= fun n -> if n=1 then false
          else check (n,n-1);;
