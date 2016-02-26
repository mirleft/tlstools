open Lwt.Infix
open Cohttp_lwt_unix

let content_type path =
  try
    let idx = String.rindex path '.' + 1 in
    match String.sub path idx (String.length path - idx) with
    | "js" -> "application/javascript"
    | "css" -> "text/css"
    | "html" -> "text/html"
    | "json" -> "application/json"
    | _ -> "text/plain"
  with _ -> "text/plain"

let response path =
  Cohttp_lwt.Response.make
    ~status:`OK
    ~headers:(Cohttp.Header.of_list [
        "Content-type" , content_type path
      ; "Connection"   , "Keep-Alive"
      ]) ()

let read f =
  Lwt_unix.access f [ Unix.F_OK ; Unix.R_OK ] >>= fun () ->
  Lwt_unix.openfile f [Unix.O_RDONLY] 0 >>= fun fd ->
  Lwt_unix.fstat fd >>= fun stats ->
  let size = stats.Lwt_unix.st_size in
  let buf = Bytes.create size in
  let rec read start =
    let len = size - start in
    Lwt_unix.read fd buf start len >>= function
    | x when x = len -> Lwt.return buf
    | x -> read (start + x)
  in
  read 0 >>= fun buf ->
  Lwt_unix.close fd >|= fun () ->
  buf

let dispatch data name =
  let fn = match name with
    | "/gui.js" -> Some "/gui.js"
    | "/sequence-diagram-min.js" -> Some "/sequence-diagram-min.js"
    | "/index.html" -> Some "/index.html"
    | "/style.css" -> Some "/style.css"
    | "/html5.js" -> Some "/html5.js"
    | "/jquery-1.11.1.min.js" -> Some "/jquery-1.11.1.min.js"
    | "/underscore-min.js" -> Some "/underscore-min.js"
    | "/raphael-min.js" -> Some "/raphael-min.js"
    | "/diagram.json" -> None
    | _ -> Some "/index.html"
  in
  match fn with
  | Some fn ->
    read (Filename.concat "data" fn) >|= fun buf ->
    (response fn, `String buf)
  | None ->
    Lwt.return (response ".json", `String data)

let server filename =
  let callback data _conn req _body =
    let uri = req |> Request.uri |> Uri.path in
    (*    let meth = req |> Request.meth |> Code.string_of_method in *)
    dispatch data uri
  in
  let sexps = Sexplib.Sexp.load_sexps filename in
  let traces = Visualisation.filter_map ~f:Visualisation.Of_Sexp.to_trace sexps in
  let data = Yojson.to_string (`List (List.map Visualisation.to_json traces)) in
  Printf.printf "listening on port 8000\n" ;
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback:(callback data) ())

let () =
  try
    match Sys.argv with
    | [| _ ; filename |] -> Lwt_main.run (server filename)
    | args -> Printf.eprintf "%s <filename>\n%!" args.(0)
  with
    | Sys_error s -> print_endline s
