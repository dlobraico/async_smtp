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
    -> unit Deferred.Or_error.t
end

module Server : sig
  val start :
    Reader.t 
    -> Writer.t
    -> Socket.Address.Inet.t
    -> rewrites:Rule.Rewrite.t list
    -> routes:Rule.Route.t list
    -> unit Deferred.t

  val start_tcp :
    port:int
    -> rewrites:Rule.Rewrite.t list
    -> routes:Rule.Route.t list
    -> unit Deferred.t
end
