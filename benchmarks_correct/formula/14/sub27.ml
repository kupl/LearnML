(*2011-10478 Shin Changho*)

type formula= TRUE
            | FALSE
            | NOT of formula
            | ANDALSO of formula*formula
            | ORELSE of formula*formula
            | IMPLY of formula*formula
            | LESS of expr*expr
and expr = NUM of int
         | PLUS of expr*expr
         | MINUS of expr*expr

let rec eval (f:formula) : bool =
        let rec value n=
                match n with
                |NUM x->x
                |PLUS (x1,x2)->(value x1)+(value x2)
                |MINUS (x1,x2)->(value x1)-(value x2) in
        match f with
        |TRUE->true
        |FALSE->false
        |NOT s->not (eval s)
        |ANDALSO (f1,f2)->(eval f1)&&(eval f2)
        |ORELSE (f1,f2)->(eval f1)||(eval f2)
        |IMPLY (f1,f2)->(not (eval f1))||(eval f2)
        |LESS (f1,f2)->(value f1)<(value f2)



