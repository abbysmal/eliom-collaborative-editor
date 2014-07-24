open Eliom_content
open Html5.D
open Html5.F

let format_page _ =
  let er = Eliom_reference.eref ~scope:Eliom_common.site_scope
  (Types.new_document "toto") in
  let ed = Client.create () in
  Client.init_and_register ed er;
  let elt = Client.get_elt ed in
  (Eliom_tools.F.html
     ~title:"DiffSync Editor"
     ~css:[["css";"editor.css"];["css";"bootstrap.css"];["css";"bootstrap-theme.css"]]
     ~js:[["js";"libs.js"]]
     (body [
         div [h1 [pcdata "Collaborative editor"]];
         div[elt]
       ]))
