{shared{

open Eliom_content
open Html5.D
open Eliom_lib.Lwt_ops
open Types

(* Type of an editor, a DOM node containing the editor itself and the bus used to communicate *)
type t =
  (Html5_types.textarea Eliom_content.Html5.D.elt * (bus_message, bus_message) Eliom_bus.t)

}}

{server{

(* FIXME: Refused doesn't need to return this *)
(* Applied return the new revision number and a clean shadow copy *)
let send_patch =
  Eliom_service.Ocaml.post_coservice'
    ~rt:(Eliom_service.rt :
           [`Applied of int * string | `Refused of int * string]
             Eliom_service.rt)
    ~post_params: (Eliom_parameter.ocaml "lol" Json.t<request>)
    ()

let patches_bus = Eliom_bus.create
    ~scope:Eliom_common.site_scope Json.t<bus_message>

}}

{client{

module Html = Dom_html
let (>>=) = Lwt.bind
open Dom

(* Status of a client: Init if the bus isn't active (and will poll the patches received from the Bus) *)
(* Ok if the bus is connected *)
(* FIXME Disconnect never happens. *)
type phase =
  | Init of (int * diff * int) list
  | Ok
  | Disconnected

(* Function to load a document when the page is loaded *)
let load_document editor old rev =
  Eliom_client.call_ocaml_service ~service:%Services.get_document () ()
  >>= fun response ->
  begin
    match response with
    | `Result (document, id) ->
      editor##value <- (Js.string document);
      old := (Js.string document);
      rev := id; Lwt.return Ok
    | `NotConnected -> Lwt.return Disconnected
  end


let make_diff text old_text rev client_id =
  let dmp = DiffMatchPatch.make () in
  let diff = DiffMatchPatch.diff_main dmp old_text text in
  {from_revision = rev; diffs = (Array.to_list diff); client = client_id;}


let get_cursor_position editor =
  let sel_start = editor##selectionStart in
  let sel_end = editor##selectionEnd in
  let text = Js.to_string (editor##value) in
  let length = String.length text in
  let end_cut =
    if sel_end > sel_start then sel_end else
      if sel_start > (length - 3) then
        1
      else
        3
  in
  let pattern = if sel_start + 1 >= length then "" else String.sub text sel_start (end_cut - sel_start) in
  sel_start, sel_end, pattern

let set_cursor_position dmp editor (sel_start, sel_end, pattern) =
  let length = editor##value##length in
  let new_start = DiffMatchPatch.match_main dmp (Js.to_string (editor##value)) pattern sel_start in
  let new_end =
    if sel_start = sel_end then new_start
    else new_start + (sel_end - sel_start) in
  editor##selectionStart <- new_start ;
  editor##selectionEnd <- new_end

(* FIXME: horrible duplication *)
(* Applies the patches polled during the Init phase *)
let apply_patches rev editor shadow_copy patches =
  List.iter (fun (id, diff, prev) ->
      if prev = !rev then
        let loc = get_cursor_position editor in
        let dmp = DiffMatchPatch.make () in
        let patch_scopy = DiffMatchPatch.patch_make dmp (Js.to_string !shadow_copy) diff in
        let patch_editor = DiffMatchPatch.patch_make dmp (Js.to_string editor##value) diff in
        editor##value <- Js.string @@ DiffMatchPatch.patch_apply dmp patch_editor (Js.to_string editor##value);
        shadow_copy := Js.string @@ DiffMatchPatch.patch_apply
            dmp patch_scopy (Js.to_string !shadow_copy);
        rev := prev
    ) (List.rev patches)

(* Apply a single patch *)
let apply_update rev editor shadow_copy diff prev =
  let loc = get_cursor_position editor in
  let current_text = Js.to_string editor##value in
  let dmp = DiffMatchPatch.make () in
  let patch_scopy = DiffMatchPatch.patch_make dmp (Js.to_string !shadow_copy) diff in
  let patch_editor = DiffMatchPatch.patch_make dmp (current_text) diff in
  editor##value <- Js.string @@ DiffMatchPatch.patch_apply dmp patch_editor current_text;
  set_cursor_position dmp editor loc;
  shadow_copy := Js.string @@ DiffMatchPatch.patch_apply
      dmp patch_scopy (Js.to_string !shadow_copy);
  rev := prev


let onload patches_bus editor_elt () =
  Random.self_init ();

  (* Is the current revision server-side *)
  let editor = Eliom_content.Html5.To_dom.of_textarea editor_elt in
  let shadow_copy = ref (Js.string "") in
  (* Is the revision number of this client *)
  let rev = ref 0 in
  (* this client id *)
  let client_id = Random.int 4096 in
  let phase = ref (Init []) in

  let is_ok _ = match !phase with
    | Ok -> true
    | _ -> false in

  Lwt.async (fun _ -> Lwt_stream.iter
  (function
    | Hello id -> (* First, check if the bus is running
                     by checking our own Hello message *)
      if id = client_id then
        begin
          match !phase with
          | Init msg_buffer -> ignore begin
              load_document editor shadow_copy rev
              >>= function
              | Ok -> Lwt.return (phase := Ok)
              | _ -> Lwt.return (phase := Disconnected)
            end
          | _ -> ()
        end
    | Patch (id, diff, prev) when prev = (!rev + 1) ->
      begin
        try
          begin
            if id != client_id && is_ok () then
              begin
                apply_update rev (Obj.magic editor) shadow_copy diff prev
              end
            else if id != client_id then
              begin
                match !phase with
                | Init l -> phase := Init ((id, diff, prev)::l)
                | _ -> ()
              end
          end
        with
        | _ -> ()
      end
    | _ -> ()
  )
  (Eliom_bus.stream patches_bus));
  ignore(Eliom_bus.write patches_bus (Hello (client_id)));

  (* changes handler *)
  Lwt_js_events.(
    async
    (fun () ->
        inputs Dom_html.document
          (fun ev _ ->
             Lwt_js.sleep 0.3
             >>= fun () ->
             let diff = make_diff (Js.to_string editor##value)
                 (Js.to_string !shadow_copy) !rev client_id in
             Eliom_client.call_ocaml_service ~service:%send_patch () diff
             >>= fun response ->
             begin
               match response with
               | `Applied (srev, scopy) -> rev := srev;
                 shadow_copy := (Js.string scopy); Lwt.return_unit
               | `Refused (srev, scopy) -> Lwt.return ()
             end
          )))
}}
