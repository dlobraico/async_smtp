open Core.Std
open Async.Std
module Octet_stream = Email_message.Octet_stream

open Common
open Types

let rec read_data buffer reader =
  Reader.read_line reader >>= fun line ->
  match line with
  | `Eof -> return ()
  | `Ok "." -> return ()
  | `Ok s ->
     Bigbuffer.add_string buffer s;
     Bigbuffer.add_char buffer '\n';
     read_data buffer reader >>= fun () -> return ()

let rec speak_smtp partner_name reader writer =
  fun first_message ~sender ~receivers ~email ->
  let exchange reply = exchange reader writer (Replies.to_string reply) in
  let speak_smtp = speak_smtp partner_name reader writer in
  let recurse = speak_smtp ~sender ~receivers ~email in
  exchange first_message >>= fun command_string ->
  match Commands.of_string_opt command_string with
  | None -> recurse Replies.(Really_bad Command_not_recognized,"")
  | Some x -> begin
              match x with
              | Commands.Hello _name -> recurse Replies.(Ok Ok_completed, partner_name)
              | Commands.Sender email_addr -> 
                 begin
                   match sender with
                   | None -> 
                      speak_smtp 
                        ~sender:(Some email_addr) 
                        ~receivers 
                        ~email 
                        Replies.(Ok Ok_completed,"")
                   | Some _ -> recurse Replies.(Really_bad Bad_sequence_of_commands,"")
                 end
              | Commands.Receiver email_addr -> 
                 speak_smtp 
                   ~sender 
                   ~receivers:(email_addr::receivers) 
                   ~email 
                   Replies.(Ok Ok_completed,"")
              | Commands.Data -> 
                 begin
                   match email with
                   | Some _ -> recurse Replies.(Really_bad Bad_sequence_of_commands,"")
                   | None ->
                      match (sender,receivers) with
                      | (None,[])
                      | (Some _, [])
                      | (None, _) -> recurse Replies.(Really_bad Bad_sequence_of_commands,"")
                      | (Some send, receiv) ->
                         exchange Replies.(Ok Start_mail_input,"") >>= fun first_line ->
                         let buffer = Bigbuffer.create (String.length first_line) in
                         Bigbuffer.add_string buffer first_line;
                         Bigbuffer.add_char buffer '\n';
                         read_data buffer reader >>= fun () ->
                         let email_msg =
                           Email_message.Email.of_octet_stream
                             (Octet_stream.of_bigstring (Bigbuffer.volatile_contents buffer))
                         in
                         let id_ = make_id send receiv email_msg in
                         speak_smtp
                           ~sender ~receivers
                           ~email:(Some (id_,email_msg))
                           Replies.(Ok Ok_completed, id_)
                 end
              | Commands.Quit ->
                 exchange Replies.(Ok Closing_connection,"") >>= fun _ ->
                 return (sender,receivers,email)
              | Commands.Help -> recurse Replies.(Ok Help,"")
              | Commands.Noop -> recurse Replies.(Ok Ok_completed,"")
            end

let start_connection partner_name reader writer =
  speak_smtp partner_name reader writer
             Replies.(Ok Service_ready,"")
                       ~sender:None
                       ~receivers:[]
                       ~email:None
  >>| fun (from,to_,imsg) ->
  match from with
  | None -> None
  | Some sender_addr ->
     match to_ with
     | [] -> None
     | receiver_addrs ->
        match imsg with
        | None -> None
        | Some (id_,email) -> 
           Some
             ( sender_addr
             , receiver_addrs
             , id_
             , email)
;;

module Router = struct
  let __UNUSED_VALUE__make_id sender receivers _email =
    let now = Time.hash (Time.now ()) in
    let send = String.hash sender in
    let receivs = String.hash (List.to_string ~f:String.to_string receivers) in
    (Int.to_string now) ^ (Int.to_string send) ^ (Int.to_string receivs)

  let send_email ~host ~port ~from:sender ~to_:receivers ~id_ message =
    Log.Global.info "%s => (from:%s to:%s) at %s:%d"
                    id_ sender (List.to_string ~f:String.to_string receivers) host port;
    let destination = Tcp.to_host_and_port host port in
    Tcp.with_connection destination 
                        (fun _socket reader writer ->
                         Client.send_email reader writer ~sender ~receivers message)
  ;;
                        
  let rewrite rules msg: smtp_email list =
    Array.fold rules ~init:[msg] 
               ~f:(fun msgs rule ->
                   (List.map msgs
                             ~f:(fun msg -> 
                                 match rule msg with
                                 | None -> [msg]
                                 | Some msgs -> msgs)) 
                   |> List.concat)
  ;;

  let route (rules: routing_rule array) msg: (string * int) list =
    Array.fold rules ~init:[]
               ~f:(fun dests rule ->
                   match rule msg with
                   | None -> dests
                   | Some dsts -> List.concat [dests; dsts])
  ;;

  let rules_server rewriting_rules routing_rules =
    let rewriting_array = Array.of_list rewriting_rules in
    let routing_array = Array.of_list routing_rules in
    let handler address reader writer =
      let partner =
        match address with
        | `Inet (inet, _) -> (Unix.Inet_addr.to_string inet)
        | `Unix s -> s
      in
      Log.Global.info "Connection from %s" partner;
      Server.start_connection partner reader writer 
      >>= function
        | None ->
           return (Log.Global.error "This shouldn't happen: no email (smtp server)")
        | Some ((_,_,id_,_) as msg) ->
           Log.Global.info "%s <= from %s" id_ partner;
           (List.concat
              (List.map 
                 (rewrite rewriting_array msg)
                 ~f:(fun msg ->
                     let from, to_, id_, email = msg in
                     List.map 
                       (route routing_array msg)
                       ~f:(fun (host,port) ->
                           send_email
                             ~host ~port
                             ~from ~to_ ~id_
                             (Email_message.Email.to_string email)
                           >>| function
                             | true  -> 
                                Log.Global.info "sent email to %s:%d" host port
                             | false -> 
                                Log.Global.error "failed to send email to %s:%d" host port))))
           |> Deferred.all
    in
    (fun s r w -> (handler s r w) >>= fun _ -> return ())
  ;;
end
