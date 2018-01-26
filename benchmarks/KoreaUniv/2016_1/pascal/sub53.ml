let rec pascal : int * int -> int
= fun (n1, n2) ->
 
   match n1 with 
   0 -> 1
   |1 -> 1     
   |_ ->if n2=0||n2=n1  then 1   else pascal(n1-1,n2-1) + pascal(n1-1,n2)

