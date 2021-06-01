exception Error of string

let rec sigma f a b =
        if a>b then raise (Error "Garbage In")
        else
           match b-a with
             0 -> f(b)
            |_ -> f(a) + sigma f (a+1) b;;