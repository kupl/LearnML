(*5*)
let rec dfact : int->int = fun n->
if n mod 2=0 then(let k=(n/2) in if k>0 then n*dfact(n-2) else 1)    
    else (let j=((n+1)/2) in if j>0 then n*dfact(n-2) else 1);;