type url = string

type attachment =
  | Video of (url * string option)
  | Image of (url * string option)

type create_obj = {
  content : string;
  summary : string option;
  sensitive : bool;
  attachments : attachment list;
  in_reply_to : url option;
  (* TODO: Detect toot privacy *)
}

type announce_obj = {
  url : url;
}

type typ =
  | Create of create_obj
  | Announce of announce_obj

type item = {
  typ : typ;
  published : (Ptime.t * Ptime.tz_offset_s option);
}

let get_attachment mime l =
  let url =
    match List.Assoc.get_exn ~eq:String.equal "url" l with
    | `String s -> s
    | _ -> assert false
  in
  let name =
    match List.Assoc.get_exn ~eq:String.equal "name" l with
    | `String s -> Some s
    | `Null -> None
    | _ -> assert false
  in
  match String.split_on_char '/' mime with
  | ["image"; _] -> Image (url, name)
  | ["video"; _] -> Video (url, name)
  | _ -> assert false

let get_attachment l =
  match List.Assoc.get_exn ~eq:String.equal "mediaType" l with
  | `String mime -> get_attachment mime l
  | _ -> assert false

let get_attachment = function
  | `O l -> get_attachment l
  | _ -> assert false

let create_create_obj l =
  let content =
    match List.Assoc.get_exn ~eq:String.equal "content" l with
    | `String s -> s
    | _ -> assert false
  in
  let summary =
    match List.Assoc.get_exn ~eq:String.equal "summary" l with
    | `String s -> Some s
    | `Null -> None
    | _ -> assert false
  in
  let sensitive =
    match List.Assoc.get_exn ~eq:String.equal "sensitive" l with
    | `Bool b -> b
    | _ -> assert false
  in
  let attachments =
    match List.Assoc.get_exn ~eq:String.equal "attachment" l with
    | `A l -> List.map get_attachment l
    | _ -> assert false
  in
  let in_reply_to =
    match List.Assoc.get_exn ~eq:String.equal "inReplyToAtomUri" l with
    | `String s -> Some s
    | `Null -> None
    | _ -> assert false
  in
  {content; summary; sensitive; attachments; in_reply_to}

let create_create_obj l =
  match List.Assoc.get_exn ~eq:String.equal "object" l with
  | `O l -> create_create_obj l
  | _ -> assert false

let get_first_cc l =
  match List.Assoc.get_exn ~eq:String.equal "cc" l with
  | `A (`String url :: _) -> url
  | _ -> assert false

let get_gnu_social_url url id =
  match String.split_on_char '/' url with
  | [scheme; ""; instance; "user"; _] -> scheme^"//"^instance^"/notice/"^id
  | _ -> assert false

let create_old_announce_obj l objectId =
  match String.split_on_char '=' objectId with
  | ["objectId"; id] -> get_first_cc l ^ "/statuses/" ^ id
  | ["noticeId"; id] -> get_gnu_social_url (get_first_cc l) id
  | _ -> assert false

(* NOTE: Everything reached from this function is an heuristic and might break *)
let create_announce_obj l s =
  match String.split_on_char ':' s with
  | ["tag"; _; objectId; _] -> {url = create_old_announce_obj l objectId}
  | "http"::_ | "https"::_ -> {url = s}
  | _ -> assert false

let create_announce_obj l =
  match List.Assoc.get_exn ~eq:String.equal "object" l with
  | `String s -> create_announce_obj l s
  | _ -> assert false

let parse_time s =
  match Ptime.of_rfc3339 s with
  | Ok (t, tz, _) -> (t, tz)
  | Error _ -> assert false

let create_item l =
  let typ =
    match List.Assoc.get_exn ~eq:String.equal "type" l with
    | `String "Create" -> Create (create_create_obj l)
    | `String "Announce" -> Announce (create_announce_obj l)
    | `String _ -> assert false
    | _ -> assert false
  in
  let published =
    match List.Assoc.get_exn ~eq:String.equal "published" l with
    | `String s -> parse_time s
    | _ -> assert false
  in
  {typ; published}

let parse_item = function
  | `O l -> create_item l
  | _ -> assert false

let parse_items = function
  | `A l -> List.map parse_item l
  | _ -> assert false

let parse = function
  | `A _ -> assert false
  | `O l -> parse_items (List.Assoc.get_exn ~eq:String.equal "orderedItems" l)

let view_item {typ; published = (t, tz)} =
  let open Tyxml.Html in
  let print_time = Ptime.pp_human ?tz_offset_s:tz () in
  let print_summary ~sensitive = function
    | Some summary -> [tr [td [b [pcdata "CW: "]; pcdata summary]]]
    | None when sensitive -> [tr [td [b [pcdata "Sensitive media"]]]]
    | None -> []
  in
  let print_in_reply_to = function
    | Some url -> [tr [td [b [pcdata "In reply to "]; a ~a:[a_href url] [pcdata url]]]]
    | None -> []
  in
  let print_metadata ~sensitive ~summary ~in_reply_to =
    table ~a:[a_style "border: 1px solid black; margin-top: 5px;"] (
      tr [td [b [pcdata "Tooted at "]; pcdata (Format.sprintf "%a" print_time t)]] ::
      print_summary ~sensitive summary @
      print_in_reply_to in_reply_to
    )
  in
  let print_attachment attachment =
    let a_title = function
      | Some name -> [a_title name]
      | None -> []
    in
    match attachment with
    | Image (url, name) -> img ~src:url ~alt:"/!\\ something went wrong... /!\\" ~a:(a_width 450::a_title name) ()
    | Video (url, name) -> video ~src:url ~a:(a_controls ()::a_title name) []
  in
  match typ with
  | Create {content; summary; sensitive; attachments; in_reply_to} ->
      div [
        print_metadata ~sensitive ~summary ~in_reply_to;
        Unsafe.data content;
        div (List.map print_attachment attachments);
      ]
  | Announce {url} ->
      div [
        b [pcdata "Boosted at "];
        pcdata (Format.sprintf "%a: " print_time t);
        a ~a:[a_href url] [pcdata url]
      ]

let rec sep = function
  | x::y::l -> x::Tyxml.Html.hr ()::sep (y::l)
  | l -> l

let view items =
  let open Tyxml.Html in
  let charset = meta ~a:[a_charset "utf-8"] () in
  let head = head (title (pcdata "mastodon-archive-viewer")) [charset] in
  let html = html head (body (sep (List.map view_item items))) in
  Format.printf "%a\n" (pp ()) html

let () =
  match Sys.argv with
  | [|_; file|] -> view (parse (IO.with_in file Ezjsonm.from_channel))
  | _ -> assert false
