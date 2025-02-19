module SimpleAssocList exposing (SimpleAssocList, empty, isEmpty, mapKeyAndValue, singleton, toList, update)


type SimpleAssocList key value
    = SimpleAssocList (List ( key, value ))


empty : SimpleAssocList key value
empty =
    SimpleAssocList []


singleton : key -> value -> SimpleAssocList key value
singleton key value =
    SimpleAssocList [ ( key, value ) ]


update : key -> (Maybe value -> Maybe value) -> SimpleAssocList key value -> SimpleAssocList key value
update key fn (SimpleAssocList list) =
    case getHelp key list of
        Nothing ->
            case fn Nothing of
                Nothing ->
                    SimpleAssocList list

                Just value ->
                    SimpleAssocList (( key, value ) :: list)

        previousValue ->
            case fn previousValue of
                Nothing ->
                    SimpleAssocList (remove key list)

                Just value ->
                    SimpleAssocList (( key, value ) :: remove key list)


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


toList : SimpleAssocList key value -> List ( key, value )
toList (SimpleAssocList list) =
    list


isEmpty : SimpleAssocList key value -> Bool
isEmpty (SimpleAssocList list) =
    List.isEmpty list
