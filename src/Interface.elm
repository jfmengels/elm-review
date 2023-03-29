module Interface exposing (create)


create : (ops -> typ) -> ((rep -> typ) -> rep -> ops) -> rep -> typ
create constructor implementation =
    let
        repTyp : rep -> typ
        repTyp rep =
            constructor (implementation repTyp rep)
    in
    repTyp
