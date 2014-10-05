{shared{

(** type representing a request sent to the server *)
type request = {client : int; from_revision : int; diffs : (int * string) list}
    deriving(Json)

(** Type representing a simple diff based on diff_match_patch *)
type diff = (int  * string) array
    deriving(Json)

(** Type representing a message on the bus for the Editor *)
type bus_message =
  | Patch of (int * diff * int)
  | Hello of int
        deriving(Json)

(** Type representing the potential results of a patch application *)
type patch_result =
  | Failure of string
  | Success of string

(** Patch representing the potentiel response from the server about a given patch *)
type response =
  | Applied of int
  | Rejected of (int * string) array list

(** The basic document type, representing a text *)
type doc = {id : int; text : string }

val new_document : string -> doc
}}