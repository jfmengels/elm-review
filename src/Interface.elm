module Interface exposing (..)


impl : t -> (raise -> rep -> t)
impl constructor =
    \_ _ -> constructor


wrap :
    (raise -> rep -> t)
    -> (raise -> rep -> (t -> q))
    -> (raise -> rep -> q)
wrap method pipeline =
    \raise rep -> method raise rep |> pipeline raise rep


add :
    (rep -> t)
    -> (raise -> rep -> (t -> q))
    -> (raise -> rep -> q)
add method pipeline =
    \raise rep -> method rep |> pipeline raise rep


map : (a -> b) -> (raise -> rep -> a) -> (raise -> rep -> b)
map op pipeline raise rep =
    pipeline raise rep |> op


init :
    (rep -> sealed)
    -> ((sealed -> output) -> sealed -> output)
    -> rep
    -> output
init ir rtrt i =
    let
        rt : sealed -> output
        rt r =
            rtrt rt r
    in
    rt (ir i)


create : (ops -> typ) -> ((rep -> typ) -> rep -> ops) -> rep -> typ
create constructor implementation =
    let
        repTyp : rep -> typ
        repTyp rep =
            constructor (implementation repTyp rep)
    in
    repTyp
