module Items = Map.Make (String)

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
  original_url : url;
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

type filters =
  | Boosts
  | Media_posts
  | Text_posts
  | Media_replies
  | Text_replies

let filter_eq x y = match x, y with
  | Boosts, Boosts
  | Media_posts, Media_posts
  | Text_posts, Text_posts
  | Media_replies, Media_replies
  | Text_replies, Text_replies -> true
  | (Boosts | Media_posts | Text_posts | Media_replies | Text_replies), _ -> false

let all_filters = "boosts, media-posts, text-posts, all-posts, media-replies, text-replies, all-replies and all-media"

let parse_filter = function
  | "boosts" -> [Boosts]
  | "media-posts" -> [Media_posts]
  | "text-posts" -> [Text_posts]
  | "all-posts" -> [Media_posts; Text_posts]
  | "media-replies" -> [Media_replies]
  | "text-replies" -> [Text_replies]
  | "all-replies" -> [Media_replies; Text_replies]
  | "all-media" -> [Media_posts; Media_replies]
  | filter -> Printf.eprintf "Filter '%s' non-recognised. Only %s are accepted.\n" filter all_filters; assert false

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

let get_in_reply_to l =
  match List.Assoc.get_exn ~eq:String.equal "inReplyToAtomUri" l with
  | `String s -> Some s
  | `Null -> None
  | _ -> assert false

let rec is_self_reply_rec m = function
  | None -> true
  | Some url ->
      match Items.find_opt url m with
      | Some l -> is_self_reply_rec m (get_in_reply_to l)
      | None -> false

let create_create_obj filters m l =
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
  let original_url =
    match List.Assoc.get_exn ~eq:String.equal "atomUri" l with
    | `String s -> s
    | _ -> assert false
  in
  let in_reply_to = get_in_reply_to l in
  let is_reply = not (is_self_reply_rec m in_reply_to) in
  if (List.mem ~eq:filter_eq Media_posts filters && not (List.is_empty attachments) && not is_reply) ||
     (List.mem ~eq:filter_eq Text_posts filters && List.is_empty attachments && not is_reply) ||
     (List.mem ~eq:filter_eq Media_replies filters && not (List.is_empty attachments) && is_reply) ||
     (List.mem ~eq:filter_eq Text_replies filters && List.is_empty attachments && is_reply) then
    None
  else
    Some {content; summary; sensitive; attachments; in_reply_to; original_url}

let create_create_obj filters m l =
  match List.Assoc.get_exn ~eq:String.equal "object" l with
  | `O l -> create_create_obj filters m l
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
  | ["urn"; "X-dfrn"; host; _; id] -> {url = Printf.sprintf "http://%s/display/%s" host id} (* NOTE: Friendica posts. Fourth field seems to be protocol version but doesn't seems to be relevant here *)
  | "http"::_ | "https"::_ -> {url = s}
  | _ -> Printf.eprintf "Can't detect announce object format: %s\n" s; assert false

let create_announce_obj l =
  match List.Assoc.get_exn ~eq:String.equal "object" l with
  | `String s -> create_announce_obj l s
  | _ -> assert false

let parse_time s =
  match Ptime.of_rfc3339 s with
  | Ok (t, tz, _) -> (t, tz)
  | Error _ -> assert false

let create_item filters m l =
  let open Option.Infix in
  let typ =
    match List.Assoc.get_exn ~eq:String.equal "type" l with
    | `String "Create" -> create_create_obj filters m l >|= fun obj -> Create obj
    | `String "Announce" when List.mem ~eq:filter_eq Boosts filters -> None
    | `String "Announce" -> Some (Announce (create_announce_obj l))
    | `String _ -> assert false
    | _ -> assert false
  in
  typ >|= fun typ ->
  let published =
    match List.Assoc.get_exn ~eq:String.equal "published" l with
    | `String s -> parse_time s
    | _ -> assert false
  in
  {typ; published}

let parse_item filters m = function
  | `O l -> create_item filters m l
  | _ -> assert false

let get_id l =
  match List.Assoc.get_exn ~eq:String.equal "id" l with
  | `String id -> id
  | _ -> assert false

let toots_map l =
  let aux m = function
    | `O l ->
        begin match List.Assoc.get_exn ~eq:String.equal "type" l with
        | `String "Create" ->
            begin match List.Assoc.get_exn ~eq:String.equal "object" l with
            | `O l ->
                begin match List.Assoc.get_exn ~eq:String.equal "atomUri" l with
                | `String id -> Items.add id l m
                | _ -> m
                end
            | _ -> m
            end
        | _ -> m
        end
    | _ -> m
  in
  List.fold_left aux Items.empty l

let parse_items filters = function
  | `A l -> List.filter_map (parse_item filters (toots_map l)) l
  | _ -> assert false

let parse filters = function
  | `A _ -> assert false
  | `O l -> parse_items filters (List.Assoc.get_exn ~eq:String.equal "orderedItems" l)

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
  let print_metadata ~sensitive ~summary ~in_reply_to ~original_url =
    table ~a:[a_style "border: 1px solid black; margin-top: 5px;"] (
      tr [td [b [pcdata "Tooted at "]; a ~a:[a_href original_url] [pcdata (Format.sprintf "%a" print_time t)]]] ::
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
    | Video (url, name) when Gif_detect.is_gif url -> video ~src:url ~a:(a_loop ()::a_autoplay ()::a_title name) []
    | Video (url, name) -> video ~src:url ~a:(a_controls ()::a_title name) []
  in
  match typ with
  | Create {content; summary; sensitive; attachments; in_reply_to; original_url} ->
      div [
        print_metadata ~sensitive ~summary ~in_reply_to ~original_url;
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

let main filters file =
  let filters = List.concat (List.map parse_filter filters) in
  view (parse filters (IO.with_in file Ezjsonm.from_channel))

let term =
  let ($) = Cmdliner.Term.($) in
  let doc_filters =
    Printf.sprintf
      "Filters out some categories of elements. Accepted filters are: %s. \
       This flags can be used sereval times to have several filters."
      all_filters
  in
  Cmdliner.Term.pure main $
  Cmdliner.Arg.(value & opt_all string [] & info ~doc:doc_filters ["filter-out"]) $
  Cmdliner.Arg.(required & pos 0 (some file) None & info ~docv:"FILE" [])

let info =
  Cmdliner.Term.info
    ~doc:"View your Mastodon archive offline."
    ~man:[`P "This program takes the outbox.json file contained in mastodon archives \
              and prints HTML to the standard output."]
    ~version:Config.version
    Config.name

let () = Cmdliner.Term.exit (Cmdliner.Term.eval (term, info))
