
(* XXX:
 - unclean application_data (data as ("", hex) pairs)
 - hex encoding stupid -- should be smarter for parsed handshake thingies
 - summary of parsed frames (CH: ver, x ciphers, n exts, hostname, y keyshares, SH: ver, cip, n exts)
  -> tree thingy of handshake frames (and reparse them!?)!
*)

type direction = [ `In | `Out ]

let direction_to_string = function `In -> "in" | `Out -> "out"

type data = (string * string) list

type tag = [ `KDF | `Failure | `Note ]

let tag_to_string = function
  | `KDF -> "kdf"
  | `Failure -> "failure"
  | `Note -> "note"

type layer = [ `Wire | `Crypted_record | `Plain_record ]

let layer_to_string = function
  | `Wire -> "wire"
  | `Crypted_record -> "crypted"
  | `Plain_record -> "plain"

type t = [
  | `Record of direction * string * data
  | `Raw of layer * direction * string * int * string list
  | `Note of direction option * tag * string * string
]

type trace = t list

let d_to_json xs =
  List.map (fun (k, v) -> `List [`String k ; `String v]) xs

let to_json = function
  | `Record (dir, msg, data) ->
    `Assoc [
      "event", `String "message" ; (* maybe "frame", but "message" is the old and not eager to update JS *)
      "direction", `String (direction_to_string dir) ;
      "message", `String msg ;
      "data" , `List (d_to_json data)
    ]
  | `Note (dir, tag, title, msg) ->
    let stag = tag_to_string tag in
    let dir = match dir with Some x -> direction_to_string x | None -> "none" in
    let rec parts left acc =
      match String.length left with
      | 0 -> List.rev acc
      | x when x > 24 -> parts (String.sub left 24 (x - 24)) (String.sub left 0 24 :: acc)
      | _ -> List.rev (left :: acc)
    in
    let msg = match tag with
      | `KDF ->
        let nocrypto = String.sub title 7 (String.length title - 7) in
        let t = try
            let comma = String.index nocrypto ',' in
            String.sub nocrypto 0 comma ^ "\n" ^ String.sub nocrypto (succ comma) (String.length nocrypto - succ comma)
              with Not_found -> nocrypto
        in
        t ^ "\n" ^ String.concat "\n" (parts msg [])
      | _ -> title ^ "\n" ^ msg
    in
    `Assoc [ "event", `String stag ; "direction", `String dir ; "message", `String msg ]
  | `Raw (layer, dir, msg, cnt, data) -> `Assoc [
      "event", `String "raw" ;
      "layer", `String (layer_to_string layer) ;
      "direction", `String (direction_to_string dir) ;
      "message", `String msg ;
      "count",  `String (string_of_int cnt) ;
      "data", `List (List.map (fun d -> `String d) data)
    ]

module Of_Sexp = struct
  open Sexplib.Sexp

  let rec flatten_sexp comb = function
    | Atom x  -> x
    | List xs -> List.map (flatten_sexp " ") xs |> String.concat comb

  let c_to_h c idx s =
    let v_to_h = function
      | x when x < 10 -> char_of_int (x + 48)
      | x -> char_of_int (x + 55)
    in
    let i = int_of_char c in
    let high = (0xf0 land i) lsr 4
    and low = 0x0f land i
    in
    Bytes.set s idx (v_to_h high) ;
    Bytes.set s (succ idx) (v_to_h low)

  let to_hex bytes =
    if String.length bytes = 0 then
      ""
    else
      let s = Bytes.make (String.length bytes * 3 - 1) ' ' in
      for i = 0 to pred (String.length bytes) do
        c_to_h (String.get bytes i) (i * 3) s
      done ;
      s

  let printable s r =
    let l = String.length s in
    for i = 0 to pred l do
      match Bytes.get s i with
      | x when int_of_char x > 0x20 && int_of_char x < 0x7F -> Bytes.set r i x
      | _ -> Bytes.set r i '.'
    done

  let app_data_to_string bytes =
    let r = Bytes.make (String.length bytes) ' ' in
    printable bytes r ;
    ["" , r]

  let to_hexdump data =
    let rec lines d acc =
      if d = "" then List.rev acc
      else
        let data, left =
          let l = String.length d in
          if l > 16 then String.sub d 0 16, String.sub d 16 (l - 16)
          else d, ""
        in
        let d1, d2 =
          let l = String.length data in
          if l > 8 then String.sub data 0 8, String.sub data 8 (l - 8)
          else data, ""
        in
        let p_hex d =
          let l = String.length d in
          let h = to_hex d in
          if l = 0 then
            Bytes.make 23 ' '
          else if l < 8 then
            h ^ Bytes.make ((8 - l) * 3) ' '
          else
            h
        in
        let cnt =
          let b = Bytes.make 4 ' ' in
          let f, s =
            let l = 16 * List.length acc in
            char_of_int (l lsr 8), char_of_int (l mod 256)
          in
          c_to_h f 0 b ;
          c_to_h s 2 b ;
          b
        in
        let hr1 = Bytes.make 8 ' '
        and hr2 = Bytes.make 8 ' '
        in
        printable d1 hr1 ;
        printable d2 hr2 ;
        let d = cnt ^ "  " ^ p_hex d1 ^ "  " ^ p_hex d2 ^ "  " ^ hr1 ^ " " ^ hr2 in
        lines left (d :: acc)
    in
    lines data []


  let maybe_add pre post =
    pre ^ (if String.length post > 0 then ": " ^ post else "")

  let ks_to_str ks = string_of_int (String.length ks) ^ " byte keyshare"

  let exts_dump = function
    | List [Atom "UnknownExtension" ; List [ Atom n ; Atom  value] ] ->
      ("Extension " ^ n, to_hex value)
    | List [Atom "KeyShare" ; List [ _group ; Atom ks ]] ->
      ("KeyShare", ks_to_str ks)
    | List [Atom "PreSharedKey" ; Atom psk ] ->
      ("PreSharedKey", to_hex psk)
    | List [Atom "PreSharedKey" ; List psks ] ->
      let ps = List.map (function Atom psk -> to_hex psk | _ -> assert false) psks in
      ("PreSharedKey", String.concat ", " ps)
    | List [Atom "KeyShare" ; List ks ] ->
      let ks = List.map (function List [ Atom g ; Atom ks ] -> g ^ ": " ^ ks_to_str ks | _ -> assert false) ks in
      ("KeyShare", String.concat ", " ks)
    | List [Atom tag ; sexps] ->
      (tag, flatten_sexp ", " sexps)
    | Atom tag ->
      (tag, "")
    | sexp -> ("other", to_string_hum sexp)

  let dict_dump = function
    | List [Atom "client_random" ; Atom value] -> ("client_random", to_hex value)
    | List [Atom "server_random" ; Atom value] -> ("server_random", to_hex value)
    | List [Atom "sessionid" ; List [Atom value]] -> ("sessionid", to_hex value)
    | List [Atom "sessionid" ; List []] -> ("sessionid", "")
    | List [Atom "extensions" ; List exts] ->
      let exts = List.map exts_dump exts in
      let exts = String.concat "; " (* XXX: re-add \n *) (List.map (fun (k, v) -> maybe_add k v) exts) in
      ("extensions", exts)
    | List [Atom tag ; sexps] -> (tag, flatten_sexp ", " sexps)
    | sexp -> ("unknown", to_string_hum sexp)

  let sexp_to_hex = function
    | Atom x -> ("", to_hex x)
    | List _ -> ("", "cannot sexp_to_hex of a list")

  let in_or_out = function "handshake-in" -> `In | "handshake-out" -> `Out | _ -> assert false
  let hs x = x = "handshake-in" || x = "handshake-out"

  let to_trace = function
    | List [Atom tag; sexps] ->
      ( match tag, sexps with
        | "wire-in", Atom data -> Some (`Raw (`Wire, `In, "raw", String.length data, to_hexdump data))
        | "wire-out", Atom data -> Some (`Raw (`Wire, `Out, "raw", String.length data, to_hexdump data))

        | "record-in", List [ List [ List [ Atom "content_type" ; Atom ty ] ; List _version ] ; Atom data ] ->
          Some (`Raw (`Crypted_record, `In, ty, String.length data, to_hexdump data))
        | "record-out", List [ Atom ty ; Atom data ] ->
          Some (`Raw (`Crypted_record, `Out, ty, String.length data, to_hexdump data))

        | "frame-out", List [ Atom ty ; Atom data ] ->
          Some (`Raw (`Plain_record, `Out, ty, String.length data, to_hexdump data))
        | "frame-in", List [ Atom ty ; Atom data ] ->
          Some (`Raw (`Plain_record, `In, ty, String.length data, to_hexdump data))

        | hs_dir, List [Atom "ClientHello"; List data ] when hs hs_dir ->
          Some (`Record (in_or_out hs_dir, "ClientHello", List.map dict_dump data))
        | hs_dir, List [Atom "ServerHello"; List data] when hs hs_dir ->
          Some (`Record (in_or_out hs_dir, "ServerHello", List.map dict_dump data))
        | hs_dir, List [Atom "EncryptedExtensions"; List data] when hs hs_dir ->
          let exts = List.map exts_dump data in
          Some (`Record (in_or_out hs_dir, "EncryptedExtensions", exts))
        | hs_dir, List [ Atom ty ; List data ] when hs hs_dir ->
          let data = List.map sexp_to_hex data in
          Some (`Record (in_or_out hs_dir, ty, data))
        | hs_dir, List [ Atom ty ; Atom data ] when hs hs_dir ->
          let data = to_hex data in
          Some (`Record (in_or_out hs_dir, ty, ["", data]))
        | hs_dir, Atom ty when hs hs_dir ->
          Some (`Record (in_or_out hs_dir, ty, []))
        | "change-cipher-spec-in", _ ->
          Some (`Record (`In, "ChangeCipherSpec", []))
        | "change-cipher-spec-out", _ ->
          Some (`Record (`Out, "ChangeCipherSpec", []))
        | "application-data-in", Atom bytes ->
          Some (`Record (`In, "ApplicationData", app_data_to_string bytes))
        | "application-data-out", Atom bytes ->
          Some (`Record (`Out, "ApplicationData", app_data_to_string bytes))
        | "alert-in", List [ Atom lvl ; Atom data ] ->
          Some (`Record (`In, "Alert", [(lvl, data)]))
        | "alert-out", List [ Atom lvl ; Atom data ] ->
          Some (`Record (`Out, "Alert", [(lvl, data)]))
        | "ok-alert-out", Atom alert ->
          Some (`Record (`Out, "ok alert", [("FATAL", alert)]))
        | "fail-alert-out", List [ Atom lvl ; Atom alert ] ->
          Some (`Record (`Out, "fail alert", [(lvl, alert)]))
        | "eof-out", _ -> Some (`Note (Some `Out, `Note, "EOF", "end of file"))

        | "failure", sexp ->
          Some (`Note (None, `Failure, "failure", to_string_hum sexp))
        | "master-secret", Atom bytes ->
          let ms =
            let rec parts left acc =
              match String.length left with
              | 0 -> List.rev acc
              | x when x > 8 -> parts (String.sub left 8 (x - 8)) (String.sub left 0 8 :: acc)
              | _ -> List.rev (left :: acc)
            in
            let hex = List.map to_hex (parts bytes []) in
            String.concat "\n" hex
          in
          Some (`Note (None, `Note, "master secret", ms))
        | "cipher", List [ Atom kex ; papr ] ->
          let pp = "KEX: " ^ kex ^ "\n" ^ (flatten_sexp " " papr) in
          Some (`Note (None, `Note, "cipher", pp))
        | "version", Atom ty -> Some (`Note (None, `Note, "version", ty))
        | x, Atom str when String.length x > 6 && String.sub x 0 6 = "crypto" ->
          Some (`Note (None, `KDF, x, to_hex str))

        | "state-in", _ -> None
        | "state-out", _ -> None
        | "buf-in", _ -> None (* happens before dropping and bailing on a record, but we've a wire-in now *)
        | _ ->
          Printf.printf "fall through inner match with %s = %s\n" tag (Sexplib.Sexp.to_string_hum sexps) ;
          None
      )
    | _ -> None
end

open Notty

let chr attr x = I.uchar attr x 1 1

let outline ?title attr i =
  let w, h = I.width i, I.height i in
  let chr = chr attr
  and hbar  = I.uchar attr 0x2500 w 1
  and vbar  = I.uchar attr 0x2502 1 h in
  let top = match title with
    | None -> hbar
    | Some img -> I.(hsnap w (void 1 1 <|> chr 0x20 <|> img <|> chr 0x20) </> hbar)
  in
  let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
  I.vcat (List.map I.hcat [ [a; top; b]; [vbar; i; vbar]; [d; hbar; c] ])

let wrap w image =
  let w1 = I.width image in
  let rec go i =
    if (w1 - i) <= w then
      [ I.hcrop i 0 image ]
    else
      I.hcrop i (w1 - i - w) image :: go (i + w)
  in
  let vs = go 0 in
  I.vcat vs

let arrow attr w msg dir =
  let hbar = I.uchar attr 0x2500 w 1
  and lbl = I.(hsnap w (void 1 1 <|> chr attr 0x20 <|> I.string attr msg <|> chr attr 0x20))
  in
  let arr = I.(lbl </> hbar) in
  match dir with
  | `In -> I.(chr attr 0x25C0 <|> arr)
  | `Out -> I.(arr <|> chr attr 0x25B6)

let color = function
  | `Wire -> A.(fg (gray 8))
  | `Crypted_record -> A.(fg (gray 12))
  | `Plain_record -> A.(fg (gray 16))

let to_term attr arrw bw = function
  | `Record (dir, msg, _data) ->
    let attr =
      if Astring.String.is_prefix ~affix:"fail" msg then
        A.(attr ++ fg red)
      else
        attr
    in
    let arrow = arrow attr arrw msg dir
    and data = I.string attr "data" (* XXX put summary here! *)
    and ovoid = I.void (2 + bw) 0
    in
    (match dir with
     | `In -> I.(ovoid <|> arrow <|> I.void 2 0 <|> I.hsnap bw data)
     | `Out -> I.(hsnap bw data <|> I.void 2 0 <|> arrow <|> ovoid))
  | `Raw (layer, dir, msg, cnt, _data) ->
    let l = layer_to_string layer in
    let attr = A.(attr ++ color layer) in
    let arrow = arrow attr arrw (l ^ " " ^ msg) dir
    and data = I.string attr (string_of_int cnt ^ " bytes " ^ l)
    and ovoid = I.void (2 + bw) 0
    in
    (match dir with
     | `In -> I.(ovoid <|> arrow <|> I.void 2 0 <|> I.hsnap bw data)
     | `Out -> I.(hsnap bw data <|> I.void 2 0 <|> arrow <|> ovoid))
  | `Note (dir, tag, title, msg) ->
    let attr = match tag with
      | `Failure -> A.(attr ++ A.fg red)
      | _ -> attr
    in
    let title = I.string attr title
    and w = match dir with
      | None -> arrw + 2 * bw
      | Some _ -> (bw - 2)
    in
    let msg =
      let elements = Astring.String.cuts ~sep:"\n" msg in
      let msg = List.map (I.string attr) elements in
      I.vcat (List.map (wrap w) msg)
    in
    let i = outline ~title attr msg in
    (match dir with
     | None -> I.hsnap (w + 2) i
     | Some `Out -> I.(hsnap bw i <|> void (4 + bw + arrw) 0)
     | Some `In -> I.(void (4 + bw + arrw) 0 <|> hsnap bw i))

let details_to_term attr bw = function
  | `Record (_dir, _msg, data) ->
    let data = List.map (fun (k, v) -> if k = "" then v else k ^ ": " ^ v) data in
    let imgs = List.map (fun x -> wrap bw (I.string attr x)) data in
    I.vcat imgs
  | `Raw (_layer, _dir, _msg, _cnt, data) ->
    I.vcat (List.map (I.string attr) data)
  | `Note _ -> I.empty

let rec filter_map ~f = function
  | []    -> []
  | x::xs ->
      match f x with
      | None    ->       filter_map ~f xs
      | Some x' -> x' :: filter_map ~f xs
