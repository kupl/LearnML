let rec zipper e=
   match e with 
     | [], [] ->  []
     |h::t,[] -> h::t
     |[],h::t -> h::t
     |h1::t1,h2::t2 -> h1+0::h2::zipper(t1,t2)