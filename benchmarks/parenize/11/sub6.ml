type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna

let glue a b = a^b

let rec parenize a =
        match a with
        |LEAF(Korea) -> "Korea"
        |LEAF(France)->"France"
        |LEAF(Usa)->"Usa"
        |LEAF(Brazil)->"Brazil"
        |LEAF(Japan)->"Japan"
        |LEAF(Nigeria)->"Nigeria"
        |LEAF(Cameroon)->"Cameroon"
        |LEAF(Poland)->"Poland"
        |LEAF(Portugal)->"Portugal"
        |LEAF(Italy)->"Italy"
        |LEAF(Germany)->"Germany"
        |LEAF(Sweden)->"Sweden"
        |LEAF(England)->"England"
        |LEAF(Croatia)->"Croatia"
        |LEAF(Argentina)->"Argentina" 

        |NODE(t,LEAF Korea) ->"("^(parenize t)^" Korea)"
        |NODE(t,LEAF France)->"("^(parenize t)^" France)"
        |NODE(t,LEAF Usa)-> "("^(parenize t)^" Usa)"
        |NODE(t,LEAF Brazil)-> "("^(parenize t)^" Brazil)"
        |NODE(t,LEAF Japan)-> "("^(parenize t)^" Japan)"
        |NODE(t,LEAF Nigeria)-> "("^(parenize t)^" Nigeria)"
        |NODE(t,LEAF Cameroon)-> "("^(parenize t)^" Cameroon)"
        |NODE(t,LEAF Poland)-> "("^(parenize t)^" Poland)"
        |NODE(t,LEAF Portugal)-> "("^(parenize t)^" Portugal)"
        |NODE(t,LEAF Italy)-> "("^(parenize t)^" Italy)"
        |NODE(t,LEAF Germany)-> "("^(parenize t)^" Germany)"
        |NODE(t,LEAF Sweden)-> "("^(parenize t)^" Sweden)"
        |NODE(t,LEAF England)-> "("^(parenize t)^" England)"
        |NODE(t,LEAF Croatia)-> "("^(parenize t)^" Croatia)"
        |NODE(t,LEAF Argentina)-> "("^(parenize t)^" Argentina)"

        |NODE(LEAF Korea,t) ->"(Korea"^(parenize t)^")"
        |NODE(LEAF France,t)-> "(France"^(parenize t)^")"
        |NODE(LEAF Usa,t)-> "(Usa"^(parenize t)^")"
        |NODE(LEAF Brazil,t)-> "(Brazil"^(parenize t)^")"
        |NODE(LEAF Japan,t)-> "(Japan"^(parenize t)^")"
        |NODE(LEAF Nigeria,t)-> "(Nigeria"^(parenize t)^")"
        |NODE(LEAF Cameroon,t)-> "(Cameroon"^(parenize t)^")"
        |NODE(LEAF Poland,t)-> "(Poland"^(parenize t)^")"
        |NODE(LEAF Portugal,t)-> "(Portugal"^(parenize t)^")"
        |NODE(LEAF Italy,t)-> "(Italy"^(parenize t)^")"
        |NODE(LEAF Germany,t)-> "(Germany"^(parenize t)^")"
        |NODE(LEAF Sweden,t)-> "(Sweden"^(parenize t)^")"
        |NODE(LEAF England,t)-> "(England"^(parenize t)^")"
        |NODE(LEAF Croatia,t)-> "(Croatia"^(parenize t)^")"
        |NODE(LEAF Argentina,t)-> "(Argentina"^(parenize t)^")"

        |NODE(x,y) -> "("^(parenize x)^(parenize y)^")"
