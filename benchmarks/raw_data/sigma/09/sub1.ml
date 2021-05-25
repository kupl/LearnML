exception Error of string

let rec sigma(a,b,f) =
        if a>b then raise (Error "Garbage In")
        else
           match b-a with
             0 -> f(b)
            |_ -> f(a) + sigma(a+1,b,f);;