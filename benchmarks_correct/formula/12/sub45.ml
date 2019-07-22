type formula  = TRUE | FALSE | NOT of formula | ANDALSO of formula * formula | ORELSE of formula * formula | IMPLY of formula * formula | LESS of expr * expr
        and expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr

let rec eval fm =
        let imp a b =
                if a = true && b = false then false
                else true
        in
        let rec exp ex =
                match ex with
                |NUM a -> a
                |PLUS (a, b) -> (exp a) + (exp b)
                |MINUS (a, b) -> (exp a) - (exp b)
        in
        match fm with
        |TRUE -> true
        |FALSE -> false
        |NOT a -> not (eval a)
        |ANDALSO (a, b) -> (eval a) && (eval b)
        |ORELSE (a, b) -> (eval a) || (eval b)
        |IMPLY (a, b) -> (imp (eval a) (eval b))
        |LESS (a, b) -> (exp a) < (exp b)
