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

  let get_document _ = get_shadowcopy ()
    >>= fun doc ->
    Lwt.return (`Result (doc.text, doc.id)) in

  Eliom_registration.Ocaml.register
    ~service:Services.get_document
    (fun () () -> get_document ());

  let elt = Client.create "" append_shadowcopy get_shadowcopy in
  Client.init_elt elt;
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
         div[Client.get_elt elt]
       ])) in

    let tmpl = format_page elt in
       Lwt.return @@ tmpl)
