{server{

(** Editor abstract type **)
type t

(** Create a new element and return the abstract type t. The parameters are setters and getters functions *)
val create : (Editor_types.doc -> unit Lwt.t) -> (unit -> Editor_types.doc Lwt.t) -> t

(** Retrieve a Dom element from a type t *)
val get_elt : t -> Html5_types.textarea Eliom_content.Html5.D.elt

(** Initialize the client code on this t element *)
val init_elt : t -> unit Eliom_pervasives.client_value

}}