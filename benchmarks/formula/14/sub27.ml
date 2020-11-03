(*2011-10478 Shin Changho*)

type formula= True
            | False
            | Not of formula
            | AndAlso of formula*formula
            | OrElse of formula*formula
            | Imply of formula*formula
            | Equal of exp*exp
and exp = Num of int
         | Plus of exp*exp
         | Minus of exp*exp

let rec eval (f:formula) : bool =
        let rec value n=
                match n with
                |Num x->x
                |Plus (x1,x2)->(value x1)+(value x2)
                |Minus (x1,x2)->(value x1)-(value x2) in
        match f with
        |True->true
        |False->false
        |Not s->not (eval s)
        |AndAlso (f1,f2)->(eval f1)&&(eval f2)
        |OrElse (f1,f2)->(eval f1)||(eval f2)
        |Imply (f1,f2)->(not (eval f1))||(eval f2)
        |Equal (f1,f2)->(value f1)=(value f2)



