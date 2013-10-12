open Core.Std
open Async.Std

let exchange reader writer = fun message ->
    Writer.write writer (message ^ "\n");
    Writer.flushed writer >>= fun () ->
    Log.Global.debug "I SAID: %s" message;
    Reader.read_line reader >>= fun s ->
    let response = match s with `Eof -> "EOF" | `Ok a -> a in
    Log.Global.debug "THEY SAID: %s" response;
    return response

