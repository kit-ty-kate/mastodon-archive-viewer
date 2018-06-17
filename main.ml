open Containers

type create_obj = {
  content : string;
  (* TODO: Use the other fields *)
}

type announce_obj = {
  url : string;
}

type typ =
  | Create of create_obj
  | Announce of announce_obj

type item = {
  typ : typ;
  published : (Ptime.t * Ptime.tz_offset_s option);
}

let create_create_obj l =
  let content =
    match List.Assoc.get_exn ~eq:String.equal "content" l with
    | `String s -> s
    | _ -> assert false
  in
  {content}

let create_create_obj l =
  match List.Assoc.get_exn ~eq:String.equal "object" l with
  | `O l -> create_create_obj l
  | _ -> assert false

let create_announce_obj s =
  if String.length s <= 4 then begin
    assert false;
  end;
  match String.sub s 0 4 with
  | "tag:" -> {url = "???"} (* TODO: Handle this case *)
  | _ -> {url = s}

let create_announce_obj l =
  match List.Assoc.get_exn ~eq:String.equal "object" l with
  | `String s -> create_announce_obj s
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
  match typ with
  | Create {content} -> p [pcdata (Format.sprintf "Tooted at %a:" print_time t); Unsafe.data content]
  | Announce {url} -> p [pcdata (Format.sprintf "Boosted at %a: " print_time t); a ~a:[a_href url] [pcdata url]]

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
