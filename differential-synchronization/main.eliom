let (>>=) = Lwt.bind

module Diffsync_app =
  Eliom_registration.App (
  struct
    let application_name = "diffsync"
  end)

let () =
  Diffsync_app.register
    ~service:Services.main_service
    (fun () () ->
       Lwt.return @@ Templates.format_page Client.content)
