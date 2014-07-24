let (>>=) = Lwt.bind

module Diffsync_app =
  Eliom_registration.App (
  struct
    let application_name = "diffsync"
  end)

let () =
  let editor = Templates.format_page () in
  Diffsync_app.register
    ~service:Services.main_service
    (fun () () ->
       Lwt.return @@ editor)
