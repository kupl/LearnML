(*컴공 2014-10618 이세영 1-1*)
let rec merge : int list*int list-> int list = function
    |([], [])-> []
    |(li1, []) ->li1
    |([], li2)->li2
    |(x::li1, y::li2)->
            if x>y then x:: (merge (li1, y::li2))
            else y:: (merge (x::li1, li2));;
