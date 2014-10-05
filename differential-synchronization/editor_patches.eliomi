{server{
	val handle_patch_request : (unit -> Editor_types.doc Lwt.t) ->
								(Editor_types.doc -> unit Lwt.t) ->
								  (Editor_types.bus_message, Editor_types.bus_message) Eliom_bus.t ->
								  Editor_types.request -> [> `Refused of (int * string) | `Applied of (int *string)] Lwt.t
}}