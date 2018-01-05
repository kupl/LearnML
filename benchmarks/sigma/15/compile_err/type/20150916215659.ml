(*2007-10575 조용훤*)
let rec sigma (a, b) f = 
  let n = fst(a, b) in
  let m = snd(a, b) in
  if n < m 
    then f n + sigma ((a + 1), b) f 
    else f n

       
      
   
