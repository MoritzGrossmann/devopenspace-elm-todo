module Models.TaskLists exposing
    ( TaskLists
    , allTaskLists
    , deleteTaskList
    , empty
    , fromList
    , get
    , insertTaskList
    , isEmpty
    , setName
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Models.TaskList as TaskList exposing (TaskList)


type TaskLists
    = TaskLists (Dict Int TaskList)


empty : TaskLists
empty =
    TaskLists Dict.empty


isEmpty : TaskLists -> Bool
isEmpty (TaskLists dict) =
    Dict.isEmpty dict


get : TaskList.Id -> TaskLists -> Maybe TaskList
get lId (TaskLists dict) =
    Dict.get (TaskList.idToInt lId) dict


fromList : List TaskList -> TaskLists
fromList lists =
    let
        dict =
            lists
                |> List.map (\list -> ( TaskList.idToInt list.id, list ))
                |> Dict.fromList
    in
    TaskLists dict


allTaskLists : TaskLists -> List TaskList
allTaskLists =
    toList


toList : TaskLists -> List TaskList
toList (TaskLists dict) =
    Dict.values dict


insertTaskList : TaskList -> TaskLists -> TaskLists
insertTaskList list (TaskLists dict) =
    TaskLists (Dict.insert (TaskList.idToInt list.id) list dict)


deleteTaskList : TaskList.Id -> TaskLists -> TaskLists
deleteTaskList lId =
    updateTaskList (always Nothing) lId
        >> Tuple.second


setName : String -> TaskList.Id -> TaskLists -> ( Maybe TaskList, TaskLists )
setName name =
    updateTaskList (Maybe.map (\list -> { list | name = name }))


updateTaskList : (Maybe TaskList -> Maybe TaskList) -> TaskList.Id -> TaskLists -> ( Maybe TaskList, TaskLists )
updateTaskList update lId (TaskLists dict) =
    let
        listNr =
            TaskList.idToInt lId

        foundList =
            Dict.get listNr dict

        updatedList =
            update foundList

        newDict =
            case updatedList of
                Nothing ->
                    Dict.remove listNr dict

                Just list ->
                    Dict.insert listNr list dict
    in
    ( updatedList, TaskLists newDict )
