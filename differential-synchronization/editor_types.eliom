{shared{
type request = {client : int; from_revision : int; diffs : (int * string) list}
    deriving(Json)

type diff = (int  * string) array
    deriving(Json)

type bus_message =
  | Patch of (int * diff * int)
  | Hello of int
        deriving(Json)

type patch_result =
  | Failure of string
  | Success of string

type response =
  | Applied of int
  | Rejected of (int * string) array list

type doc = {id : int; text : string }

let new_document text = {id = 0; text = text}
}}
