open Core.Std
open Async.Std
open Types

module Client : sig
  val send_email :
    Reader.t
    -> Writer.t
    -> sender:string
    -> receivers:string list
    -> string
    -> bool Deferred.t
end

module Server : sig
  val start_connection :
    string
    -> Reader.t
    -> Writer.t
    -> Message.t option Deferred.t

  val rules_server :
    Rule.Rewrite.t list
    -> Rule.Route.t list
    -> Socket.Address.Inet.t
    -> Reader.t 
    -> Writer.t
    -> unit Deferred.t
end
