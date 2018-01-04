type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna
let team_to_string team=match team with
        |Korea->"Korea"|France->"France"|Usa->"Usa"|Brazil->"Brazil"|Japan->"Japan"|Nigeria->"Nigeria"|Cameroon->"Cameroon"|Poland->"Poland"|Portugal->"Portugal"|Italy->"Italy"|Germany->"Germany"|Sweden->"Sweden"|England->"England"|Croatia->"Croatia"|Argentina->"Argentina"
let rec parenize trn=
        match trn with
        |LEAF(tm)->team_to_string(tm)
        |NODE(a,b)->String.concat "" ["(";(parenize(a));" ";(parenize(b));")"]
