open Lwt.Infix
open Visualisation
open Notty

type mode = [ `NoWire | `NoCrypt | `NoPlain | `NoParse | `NoKDF ]

type state = {
  flname : string ;
  traces : trace ;
  active : int ;
  filter : mode list ;
  detail : int ;
  dscrol : int ;
}

let filter state t =
  let filterone = function
    | `NoWire -> (function `Raw (`Wire, _, _, _, _) -> false | _ -> true)
    | `NoCrypt -> (function `Raw (`Crypted_record, _, _, _, _) -> false | _ -> true)
    | `NoPlain -> (function `Raw (`Plain_record, _, _, _, _) -> false | _ -> true)
    | `NoParse -> (function `Record _ -> false | _ -> true)
    | `NoKDF -> (function `Note (_, `KDF, _, _) -> false | _ -> true)
  in
  List.for_all (fun f -> filterone f t) state.filter

let take x lst =
  let rec take acc x xs = match x, xs with
    | _, [] -> List.rev acc
    | 0, _ -> List.rev acc
    | n, x::xs -> take (x::acc) (pred n) xs
  in
  take [] x lst

let rec drop x lst =
  match x, lst with
  | 0, lst -> lst
  | _, [] -> []
  | n, _::xs -> drop (pred n) xs

let last w state =
  let a = A.(bg lightblue) in
  let f_to_s = "[" ^
    (if List.mem `NoWire state.filter then "-" else "W") ^
    (if List.mem `NoCrypt state.filter then "-" else "C") ^
    (if List.mem `NoPlain state.filter then "-" else "P") ^
    (if List.mem `NoParse state.filter then "-" else "H") ^
    (if List.mem `NoKDF state.filter then "-" else "K") ^ "]"
  in
  let vbar = I.uchar a 0x2500 w 1
  and fn = I.string a state.flname
  and f = I.string a f_to_s
  in
  I.(fn </> hsnap ~align:`Right w f </> vbar)

let render_state (w, h) state =
  let arrw, bw =
    let aw = min 40 (w / 3) in
    (aw, (w - aw - 4) / 2)
  and mh, dh =
    let h = h - 1 in
    if h < state.detail then h, 0 else h - state.detail, state.detail
  in
  let details =
    let full = details_to_term A.empty w (List.nth state.traces state.active) in
    let topcut = state.dscrol * dh in
    let h = I.height full in
    let cut =
      if dh < h then
        I.vcrop topcut 0 full
      else
        full
    in
    I.vsnap ~align:`Top dh cut
  and main =
    let f_tt xs = List.map (to_term A.empty arrw bw) (List.filter (filter state) xs) in
    let t = state.traces in
    let before = f_tt (take state.active t)
    and act = to_term A.(st reverse) arrw bw (List.nth t state.active)
    and after = f_tt (drop (succ state.active) t)
    in
    let thing =
      let ab = List.fold_left (fun a i -> a + I.height i) 0 before
      and be = List.fold_left (fun a i -> a + I.height i) 0 after
      in
      let have = mh - I.height act in
      if ab + be > have then
        let ab, be =
          let halve = have / 2 in
          if ab < halve then
            ab, have - ab
          else if be < halve then
            have - be, be
          else
            have - halve, halve
        in
        let rec take_x have acc = function
          | [] -> (acc, None)
          | _ when have = 0 -> (acc, None)
          | x::xs when I.height x <= have -> take_x (have - I.height x) (x :: acc) xs
          | x::_ -> (acc, Some (x, have))
        in
        let above, alast = take_x ab [] (List.rev before)
        and below, blast = take_x be [] after
        in
        let top = match alast with
          | Some (a, h) -> I.vcrop (I.height a - h) 0 a :: above
          | None -> above
        and bot = match blast with
          | Some (b, h) -> I.vcrop 0 (I.height b - h) b :: below
          | None -> below
        in
        top @ act :: List.rev bot
      else
        before @ act :: after
    in
    I.vsnap mh (I.vcat thing)
  in
  I.vcat [ main ; last w state ; details ]

let next state =
  let act = ref (-1) in
  List.iteri
    (fun idx -> function
       | `Note _ -> ()
       | x -> if filter state x && idx > state.active && !act = -1 then act := idx)
    state.traces ;
  if !act = -1 then state.active else !act

let prev state =
  let act = ref 0 in
  List.iteri (fun idx -> function
      | `Note _ -> ()
      | x -> if filter state x && idx < state.active then act := idx)
    state.traces ;
  !act

let rec main_loop term size state =
  let img = render_state size state in
  Notty_lwt.Term.image term img >>= fun () ->
  Lwt_stream.next (Notty_lwt.Term.events term) >>= fun e ->
  match e with
  | `Key (`Uchar 0x44, [`Ctrl]) (* C-d *) -> Lwt.return_unit
  | `Key (`Page `Up, []) -> main_loop term size { state with dscrol = max 0 (pred state.dscrol) }
  | `Key (`Page `Down, []) -> main_loop term size { state with dscrol = succ state.dscrol }
  | `Key (`Function 11, []) ->
    let detail = min (succ state.detail) (snd size) in
    main_loop term size { state with detail }
  | `Key (`Function 11, _) ->
    let detail = max (pred state.detail) 0 in
    main_loop term size { state with detail }
  | `Key (`Arrow `Up, []) -> main_loop term size { state with active = prev state ; dscrol = 0 }
  | `Key (`Arrow `Down, []) -> main_loop term size { state with active = next state ; dscrol = 0 }
  | `Key (`Uchar chr, []) ->
    let f = match chr with
      | 0x77 -> Some `NoWire
      | 0x63 -> Some `NoCrypt
      | 0x70 -> Some `NoPlain
      | 0x68 -> Some `NoParse
      | 0x6b -> Some `NoKDF
      | _ -> None
    in
    (match f with
     | None -> main_loop term size state
     | Some f ->
       let state =
         if List.mem f state.filter then
           { state with filter = List.filter (fun x -> x <> f) state.filter }
         else
           { state with filter = f :: state.filter }
       in
       main_loop term size state)
  | `Resize size -> main_loop term size state
  | _ -> main_loop term size state

let main flname =
  let sexps = Sexplib.Sexp.load_sexps flname in
  let traces = filter_map ~f:Of_Sexp.to_trace sexps in
  let state = { flname ; traces ; active = 0 ; filter = [`NoKDF; `NoWire; `NoPlain; `NoCrypt] ; detail = 10 ; dscrol = 0 } in
  let state = { state with active = next state } in
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_unix.tcgetattr Lwt_unix.stdin >>= fun saved_tc ->
  let term_cleanup = Lwt_unix.(tcsetattr stdin TCSANOW saved_tc) in
  Lwt_unix.(tcsetattr stdin TCSANOW { saved_tc with c_ignbrk = true ; c_isig = false ; c_icanon = false ; c_echo = false ; c_vstart = '\255' ; c_vstop = '\255' ; c_verase = '\255' }) >>= fun () ->
  let term = Notty_lwt.Term.create ~input:Lwt_unix.stdin ~mouse:false () in
  let size = Notty_lwt.Term.size term in
  main_loop term size state >>= fun () ->
  term_cleanup

let () =
  try
    match Sys.argv with
    | [| _ ; filename |] -> Lwt_main.run (main filename)
    | args -> Printf.eprintf "%s <filename>\n%!" args.(0)
  with
  | Sys_error s -> print_endline s
