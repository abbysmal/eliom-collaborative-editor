let (>>=) = Lwt.bind
open Eliom_content
open Html5.D
open Html5.F

module Diffsync_app =
  Eliom_registration.App (
  struct
    let application_name = "diffsync"
  end)

let patches_bus = Eliom_bus.create
    ~scope:Eliom_common.site_scope Json.t<Types.bus_message>

let () =
  let eref = Eliom_reference.eref ~scope:Eliom_common.site_scope
      (Types.new_document "document") in


  let append_shadowcopy, get_shadowcopy =
    ((fun elm -> Eliom_reference.set eref elm),
     (fun () -> Eliom_reference.get eref)) in

  let handler = Patches.handle_patch_request get_shadowcopy append_shadowcopy
  patches_bus in
  Eliom_registration.Ocaml.register
    ~service:Client.send_patch
    (fun () patch ->
       handler patch);

  let get_document name = get_shadowcopy ()
    >>= fun {Types.id = id; Types.text = scopy} ->
    Lwt.return (`Result (scopy, id)) in

  Eliom_registration.Ocaml.register
    ~service:Services.get_document
    (fun () () -> get_document ());

  let elt = Eliom_content.Html5.D.raw_textarea ~a:[] ~name:"editor" () in
  Diffsync_app.register
    ~service:Services.main_service
    (fun () () ->
  let format_page elt =
  (Eliom_tools.F.html
     ~title:"DiffSync Editor"
     ~css:[["css";"editor.css"];["css";"bootstrap.css"];["css";"bootstrap-theme.css"]]
     ~js:[["js";"libs.js"]]
     (body [
         div [h1 [pcdata "Collaborative editor"]];
         div[elt]
       ])) in

    let tmpl = format_page elt in
       Lwt.return @@ tmpl)
