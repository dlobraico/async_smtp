open Core.Std
open Async.Std
       
module Octet_stream = Email_message.Octet_stream
module Glog = Log.Global

open Types

let exchange reader writer = fun message ->
  Writer.write writer (message ^ "\n");
  Writer.flushed writer >>= fun () ->
  Log.Global.debug "I SAID: %s" message;
  Reader.read_line reader >>= fun s ->
  let response = match s with `Eof -> "EOF" | `Ok a -> a in
  Log.Global.debug "THEY SAID: %s" response;
  return response
;;

module Client = struct
  let rec read_whole_reply reader (strs:string list) : string list Deferred.t =
    match strs with
    | [] -> begin
            Reader.read_line reader >>= function
                                      | `Eof -> return []
                                      | `Ok s -> read_whole_reply reader [s]
          end
    | (str::_) as strs ->
       if Replies.is_last_line str then return (List.rev strs)
       else Reader.read_line reader >>= function
                                      | `Eof -> return (List.rev strs)
                                      | `Ok s -> read_whole_reply reader (s::strs)

  let is_ok (r:string Deferred.t): unit option Deferred.t = 
    r >>= fun r ->
    match Replies.of_string r with
    | (Replies.Bad _, _ )
    | (Replies.Really_bad _, _) -> return None
    | (Replies.Ok  _, _) -> return (Some ())

  let exchange reader writer message =
    exchange reader writer message >>= fun response ->
    if Replies.is_last_line response
    then return response
    else
      read_whole_reply reader [response] >>= fun strs ->
      return (String.concat strs ~sep:"\n")

  let bind_is_ok t f =
    is_ok t 
    >>= function
      | Some x -> f x
      | None -> Deferred.return None

  let send_email reader writer ~sender ~receivers message =
    let exchange_str = exchange reader writer in
    let exchange command = exchange_str (Commands.to_string command) in
    Reader.read_line reader 
    >>= function
      | `Eof -> return false
      | `Ok resp ->
         Log.Global.debug "THEY SAID: %s" resp;
         let (>>=~) = bind_is_ok in
         let result =
           return resp
           >>=~ fun () -> 
           exchange (Commands.Hello (Unix.gethostname ()))
           >>=~ fun () -> 
           exchange (Commands.Sender sender)
           >>=~ fun () -> 
           exchange (Commands.Receiver (String.concat ~sep:" " receivers)) 
           >>=~ fun () -> 
           exchange Commands.Data 
           >>=~ fun () -> 
           exchange_str (message ^ "\n.") 
           >>=~ fun () -> 
           exchange Commands.Quit 
           >>=~ fun s -> 
           return (Some s)
         in result >>= function
                     | None -> return false
                     | Some _ -> return true
end

module Server = struct
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
    fun first_command ~sender ~receivers ~email ->
    let exchange reply = exchange reader writer (Replies.to_string reply) in
    let speak_smtp = speak_smtp partner_name reader writer in
    let recurse = speak_smtp ~sender ~receivers ~email in
    exchange first_command >>= fun command_string ->
    match Commands.of_string_opt command_string with
    | None -> recurse Replies.(Really_bad Command_not_recognized,"")
    | Some x -> 
       begin
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
                    let message = Message.create ~sender:send ~receivers:receiv ~email:email_msg in
                    speak_smtp
                      ~sender ~receivers
                      ~email:(Some message)
                      Replies.(Ok Ok_completed, Message.Id.to_string (Message.id message))
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
       | receiver_addrs -> imsg 
  ;;

  let send_email ~host ~port message =
    let sender    = Message.sender message in
    let receivers = Message.receivers message in
    let id        = Message.id message in
    let email     = Message.email message |> Email_message.Email.to_string in
    Log.Global.info "%s => (from:%s to:%s) at %s:%d"
                    (Message.Id.to_string id) sender 
                    (List.to_string ~f:String.to_string receivers) host port;
    let destination = Tcp.to_host_and_port host port in
    Tcp.with_connection destination 
                        (fun _socket reader writer ->
                         Client.send_email reader writer ~sender ~receivers email)
  ;;

  let rewriter ~rules ~rewriting_r ~routing_w =
    let rewrite rules msg =
      List.fold 
        rules ~init:[msg] 
        ~f:(fun msgs rule ->
            (List.concat_map 
               msgs 
               ~f:(fun msg -> 
                   Option.value ~default:[msg] (rule msg))))
    in
    Pipe.iter 
      rewriting_r
      ~f:(fun msg -> 
          Deferred.List.iter 
            (rewrite rules msg)
            ~f:(fun msg -> Pipe.write routing_w msg))
  ;;

  let router ~rules ~routing_r ~routing_w =
    let route rules msg =
      List.fold rules ~init:[]
                ~f:(fun dests rule ->
                    match rule msg with
                    | None -> dests
                    | Some dsts -> List.concat [dests; dsts])
    in
    Pipe.iter 
      routing_r
      ~f:(fun msg -> 
          Deferred.List.iter 
            (route rules msg)
            ~f:(fun (host, port) -> 
                send_email ~host ~port msg
                >>= function
                  | true -> Deferred.unit
                  | false -> 
                     (* CR dlobraico: Use some kind of priority queue
                   here instead of just writing it back onto the
                   pipe. *)
                     Pipe.write routing_w msg))
  ;;

  let start r w addr ~rewrites ~routes =
    let (rewriting_r, rewriting_w) = Pipe.create () in
    let (routing_r, routing_w)     = Pipe.create () in
    let handler address reader writer =
      let partner =
        match address with
        | `Inet (inet, _) -> (Unix.Inet_addr.to_string inet)
        | `Unix s -> s
      in
      Log.Global.info "Connection from %s" partner;
      begin
        start_connection partner reader writer 
        >>= function
          | None ->
             return (Glog.error "This shouldn't happen: no email (smtp server)")
          | Some msg ->
             Glog.info "%s <= from %s" (Message.id msg |> Message.Id.to_string) partner;
             Pipe.write rewriting_w msg
      end
    in
    begin
      rewriter ~rules:rewrites ~rewriting_r ~routing_w
      >>> fun () -> Glog.debug "rewriter stopped"
    end;
    begin
      router ~rules:routes ~routing_r ~routing_w
      >>> fun () -> Glog.debug "router stopped"
    end;
    begin
      handler addr r w
    end
  ;;

end
