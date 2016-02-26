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

let rec take_x have acc = function
  | [] -> (acc, None)
  | _ when have = 0 -> (acc, None)
  | x::xs when I.height x <= have -> take_x (have - I.height x) (x :: acc) xs
  | x::_ -> (acc, Some (x, have))

let last a w state =
  let a = A.(bg lightblack ++ a) in
  let f_to_s = "[" ^
    (if List.mem `NoWire state.filter then "-" else "w") ^
    (if List.mem `NoCrypt state.filter then "-" else "c") ^
    (if List.mem `NoPlain state.filter then "-" else "p") ^
    (if List.mem `NoParse state.filter then "-" else "h") ^
    (if List.mem `NoKDF state.filter then "-" else "k") ^ "]"
  in
  let vbar = I.uchar a 0x2500 w 1
  and fn = I.string a state.flname
  and f = I.string a f_to_s
  and h = I.string a " press F1 for help "
  in
  I.(fn </> hsnap ~align:`Right w (h <|> void 2 1 <|> f) </> vbar)

let render_state (w, h) a state =
  let arrw, bw =
    let aw = min 40 (w / 3) in
    (aw, (w - aw - 4) / 2)
  and mh, dh =
    let h = h - 1 in
    if h < state.detail then h, 0 else h - state.detail, state.detail
  in
  let details =
    let full = details_to_term a w (List.nth state.traces state.active) in
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
    let tt a x = to_term a arrw bw x in
    let f_tt xs = List.map (tt a) (List.filter (filter state) xs) in
    let before, act, after, _ =
      List.fold_left
        (fun (b, n, a, idx) x -> match n with
           | Some _ -> (b, n, x :: a, idx)
           | None when idx = state.active -> (List.rev b, Some x, a, idx)
           | None -> (x :: b, n, a, succ idx))
        ([], None, [], 0)
        state.traces
    in
    let before = f_tt before
    and act = match act with None -> assert false | Some x -> tt A.(st reverse ++ a) x
    and after = f_tt (List.rev after)
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
  I.vcat [ main ; last a w state ; details ]

let help (w, h) =
  let b = A.(st bold)
  and a = A.empty
  and u = A.(st underline)
  in
  let top =
    let welcome = I.(string a "Welcome to " <|> string b "tlsvis" <|> string a "!") in
    I.(hsnap w welcome <-> void 0 1)
  and descr =
    let fst = I.string a "An interactive TLS trace visualisation."
    and snd = I.string a "At the top its sequence diagram,"
    and trd = I.string a "below details of the active one."
    in
    I.(hsnap w fst <-> void 0 1 <-> hsnap w snd <-> hsnap w trd)
  and gb = [
    ( "C-d" , "exit" ) ;
    ( "?, F1" , "display this help screen" ) ;
    ( "F11" , "increase details height" ) ;
    ( "C-F11" , "decrease details height" ) ]
  and tb = [
    ( "ArrowUp" , "navigate sequence diagram up" ) ;
    ( "ArrowDown" , "navigate sequence diagram down" ) ;
    ( "w" , "toggle wire layer" ) ;
    ( "c" , "toggle encrypted layer" ) ;
    ( "p" , "toggle raw plaintext layer" ) ;
    ( "h" , "toggle parsed handshake layer" ) ;
    ( "k" , "toggle KDF" ) ]
  and db = [
    ( "PageUp" , "scroll up details" ) ;
    ( "PageDown" , "scroll down details" ) ]
  in
  let kbar, dbar =
    let kl, dl = List.fold_left
        (fun (k, d) x ->
           (max k (I.width (I.string a (fst x))),
            max d (I.width (I.string a (snd x)))))
        (0, 0) (gb@tb@db)
    in
    (I.char a ' ' (succ kl) 1, I.char a ' ' (succ dl) 1)
  in
  let to_i title bs =
    let b_to_i (k, d) = I.((string b k </> kbar) <|> (string a d </> dbar)) in
    let i = I.vcat (List.map b_to_i bs) in
    let title = I.(string A.(a ++ u) title) in
    I.(title <-> i)
  and last = I.(string a "press " <|> string b "any key" <|> string a " to continue")
  and disc =
    let cl = I.uchars a [| 0x2184 ; 0x20DD |] in
    I.(hsnap w ~align:`Right (cl <|> string a " all rights reversed" <|> void 2 0) </>
       hsnap w ~align:`Left (void 2 0 <|> string a "https://nqsb.io"))
  in
  let t = [
    top ;
    descr ;
    I.(hsnap w (outline ~title:(string a "Key bindings") a
                  (to_i "Global" gb <->
                   void 0 1 <-> to_i "Sequence diagram" tb <->
                   void 0 1 <-> to_i "Details" db))) ;
    I.hsnap w last
  ]
  in
  let height = List.fold_left (fun acc i -> acc + I.height i) 0 t in
  let t =
    if height < h + 6 then
      List.flatten (List.map (fun i -> [ i ; I.void 0 1 ]) t)
    else
      t
  in
  I.(vsnap h (vcat t) </> vsnap h ~align:`Bottom (disc <-> void 0 1))

let next state =
  let act = ref (-1) in
  List.iteri
    (fun idx -> function
       | `Note _ -> ()
       | x -> if filter state x && idx > state.active && !act = -1 then act := idx)
    state.traces ;
  if !act = -1 then state.active else !act

let prev state =
  let act = ref (-1) in
  List.iteri (fun idx -> function
      | `Note _ -> ()
      | x -> if filter state x && idx < state.active then act := idx)
    state.traces ;
  if !act = -1 then
    let first = next { state with active = -1 } in
    max first 0
  else
    !act

let rec main_loop term size hlp state =
  let img =
    if hlp then
      I.(help size </> render_state size A.(fg (gray 8) ++ bg (gray 3)) state)
    else
      render_state size A.empty state
  in
  Notty_lwt.Term.image term img >>= fun () ->
  Lwt_stream.next (Notty_lwt.Term.events term) >>= fun e ->
  match e with
  | `Resize size -> main_loop term size hlp state
  | _ when hlp -> main_loop term size false state
  | `Key (`Uchar 0x44, [`Ctrl]) (* C-d *) -> Lwt.return_unit
  | `Key (`Page `Up, []) -> main_loop term size false { state with dscrol = max 0 (pred state.dscrol) }
  | `Key (`Page `Down, []) -> main_loop term size false { state with dscrol = succ state.dscrol }
  | `Key (`Function 11, []) ->
    let detail = min (succ state.detail) (snd size) in
    main_loop term size false { state with detail }
  | `Key (`Function 11, _) ->
    let detail = max (pred state.detail) 0 in
    main_loop term size false { state with detail }
  | `Key (`Arrow `Up, []) -> main_loop term size false { state with active = prev state ; dscrol = 0 }
  | `Key (`Arrow `Down, []) -> main_loop term size false { state with active = next state ; dscrol = 0 }
  | `Key (`Uchar chr, []) when List.mem chr [0x63; 0x68; 0x6b; 0x70; 0x77] ->
    let f = match chr with
      | 0x63 -> `NoCrypt
      | 0x68 -> `NoParse
      | 0x6b -> `NoKDF
      | 0x70 -> `NoPlain
      | 0x77 -> `NoWire
      | _ -> assert false
    in
    let state =
      if List.mem f state.filter then
        { state with filter = List.filter (fun x -> x <> f) state.filter }
      else
        { state with filter = f :: state.filter }
    in
    main_loop term size false state
  | `Key (`Uchar 0x3f, [])
  | `Key (`Function 1, []) -> main_loop term size true state
  | _ -> main_loop term size false state

let main flname =
  let sexps = Sexplib.Sexp.load_sexps flname in
  let traces = filter_map ~f:Of_Sexp.to_trace sexps in
  let state = { flname ; traces ; active = 0 ; filter = [`NoKDF; `NoWire; `NoPlain; `NoCrypt] ; detail = 10 ; dscrol = 0 } in
  let state = { state with active = next state } in
  Sys.(set_signal sigpipe Signal_ignore) ;

  let term = Notty_lwt.Term.create ~mouse:false () in

  let tc = Unix.(tcgetattr stdin) in
  Unix.(tcsetattr stdin TCSANOW { tc with c_isig = false ; c_ixon = false ; c_ixoff = false }) ;

  let size = Notty_lwt.Term.size term in
  main_loop term size true state

let () =
  try
    match Sys.argv with
    | [| _ ; filename |] -> Lwt_main.run (main filename)
    | args -> Printf.eprintf "%s <filename>\n%!" args.(0)
  with
  | Sys_error s -> print_endline s
