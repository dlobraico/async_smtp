open Core.Std
open Async.Std

open Common
open Types

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

let send_email reader writer ~from:sender ~to_:receivers message =
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
