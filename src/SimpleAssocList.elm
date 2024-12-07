module SimpleAssocList exposing (SimpleAssocList, empty, get, insert, isEmpty, mapKeyAndValue, singleton, toDict, toList, upsert)

import Dict exposing (Dict)


type SimpleAssocList key value
    = SimpleAssocList (List ( key, value ))


empty : SimpleAssocList key value
empty =
    SimpleAssocList []


singleton : key -> value -> SimpleAssocList key value
singleton key value =
    SimpleAssocList [ ( key, value ) ]


insert : key -> value -> SimpleAssocList key value -> SimpleAssocList key value
insert key value (SimpleAssocList list) =
    SimpleAssocList (( key, value ) :: remove key list)


upsert : key -> (Maybe value -> value) -> SimpleAssocList key value -> SimpleAssocList key value
upsert key fn (SimpleAssocList list) =
    case getHelp key list of
        Nothing ->
            SimpleAssocList (( key, fn Nothing ) :: list)

        value ->
            SimpleAssocList (( key, fn value ) :: remove key list)


mapKeyAndValue : key -> (value -> ( key, value )) -> SimpleAssocList key value -> SimpleAssocList key value
mapKeyAndValue targetKey fn (SimpleAssocList list) =
    SimpleAssocList
        (List.map
            (\(( key, value ) as untouched) ->
                if key == targetKey then
                    fn value

                else
                    untouched
            )
            list
        )


get : key -> SimpleAssocList key value -> Maybe value
get targetKey (SimpleAssocList list) =
    getHelp targetKey list


getHelp : key -> List ( key, value ) -> Maybe value
getHelp targetKey list =
    case list of
        [] ->
            Nothing

        ( key, value ) :: rest ->
            if key == targetKey then
                Just value

            else
                getHelp targetKey rest


remove : key -> List ( key, value ) -> List ( key, value )
remove targetKey list =
    List.filter (\( key, _ ) -> key /= targetKey) list


toDict : SimpleAssocList comparable value -> Dict comparable value
toDict (SimpleAssocList list) =
    Dict.fromList list


toList : SimpleAssocList key value -> List ( key, value )
toList (SimpleAssocList list) =
    list


isEmpty : SimpleAssocList key value -> Bool
isEmpty (SimpleAssocList list) =
    List.isEmpty list
