{shared{

open Eliom_content
open Html5.D
open Eliom_lib.Lwt_ops
open Editor_types


}}

{server{

type t =
  { elt : Html5_types.textarea Eliom_content.Html5.D.elt; 
    bus : (bus_message, bus_message) Eliom_bus.t;
    set : (Editor_types.doc -> unit Lwt.t); get : (unit -> Editor_types.doc Lwt.t) }

let send_patch =
  Eliom_service.Ocaml.post_coservice'
    ~rt:(Eliom_service.rt :
           [`Applied of int * string | `Refused of int * string]
             Eliom_service.rt)
    ~post_params: (Eliom_parameter.ocaml "lol" Json.t<request>)
    ()

}}

{client{

module Html = Dom_html
let (>>=) = Lwt.bind
open Dom


type phase =
  | Init of (int * diff * int) list
  | Ok
  | Disconnected


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



let apply_patches rev editor shadow_copy patches =
  List.iter (fun (id, diff, prev) ->
      if prev = !rev then
        let dmp = DiffMatchPatch.make () in
        let patch_scopy = DiffMatchPatch.patch_make dmp (Js.to_string !shadow_copy) diff in
        let patch_editor = DiffMatchPatch.patch_make dmp (Js.to_string editor##value) diff in
        editor##value <- Js.string @@ DiffMatchPatch.patch_apply dmp patch_editor (Js.to_string editor##value);
        shadow_copy := Js.string @@ DiffMatchPatch.patch_apply
            dmp patch_scopy (Js.to_string !shadow_copy);
        rev := prev
    ) (List.rev patches)


let apply_update rev editor shadow_copy diff prev =
  let current_text = Js.to_string editor##value in
  let dmp = DiffMatchPatch.make () in
  let patch_scopy = DiffMatchPatch.patch_make dmp (Js.to_string !shadow_copy) diff in
  let patch_editor = DiffMatchPatch.patch_make dmp (current_text) diff in
  editor##value <- Js.string @@ DiffMatchPatch.patch_apply dmp patch_editor current_text;
  shadow_copy := Js.string @@ DiffMatchPatch.patch_apply
      dmp patch_scopy (Js.to_string !shadow_copy);
  rev := prev


let onload editor_elt patches_bus  =
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

{server{

  let create set get =
    let bus = Eliom_bus.create ~scope:Eliom_common.site_scope Json.t<bus_message> in
    let handler = Editor_patches.handle_patch_request get set bus in
    let elt = Eliom_content.Html5.D.raw_textarea ~a:[] ~name:"editor" () in
    Eliom_registration.Ocaml.register ~service:send_patch (fun () patch -> handler patch);
    { elt; bus; set; get}

  let get_elt t = t.elt

  let init_elt t =
    let elt = t.elt in
    let bus = t.bus in
    {unit{ onload %elt %bus}}
}}