-- | Types used by the Snap HTTP Server.
module Snap.Http.Server.Types
  ( ServerConfig
  , PerSessionData
  -- * HTTP lifecycle
  -- $lifecycle

  -- * Hooks
  -- $hooks

  , DataFinishedHook
  , EscapeSnapHook
  , ExceptionHook
  , ParseHook
  , NewRequestHook
  , UserHandlerFinishedHook

  -- * Handlers
  , SendFileHandler
  , ServerHandler
  , AcceptFunc

  -- * Socket types
  , SocketConfig(..)
  ) where

import           Snap.Internal.Http.Server.Types

                          ---------------------------
                          -- snap server lifecycle --
                          ---------------------------

------------------------------------------------------------------------------
-- $lifecycle
--
-- 'Request' \/ 'Response' lifecycle for \"normal\" requests (i.e. without
-- errors):
--
-- 1. accept a new connection, set it up (e.g. with SSL)
--
-- 2. create a 'PerSessionData' object
--
-- 3. Enter the 'SessionHandler', which:
--
-- 4. calls the 'NewRequestHook', making a new hookState object.
--
-- 5. parses the HTTP request. If the session is over, we stop here.
--
-- 6. calls the 'ParseHook'
--
-- 7. enters the 'ServerHandler', which is provided by another part of the
--    framework
--
-- 8. the server handler passes control to the user handler
--
-- 9. a 'Response' is produced, and the 'UserHandlerFinishedHook' is called.
--
-- 10. the 'Response' is written to the client
--
-- 11. the 'DataFinishedHook' is called.
--
-- 12. we go to #3.


                                  -----------
                                  -- hooks --
                                  -----------

------------------------------------------------------------------------------
-- $hooks
-- #hooks#
--
-- At various critical points in the HTTP lifecycle, the Snap server will call
-- user-defined \"hooks\" that can be used for instrumentation or tracing of
-- the process of building the HTTP response. The first hook called, the
-- 'NewRequestHook', FIXME FIXME FIXME will generate a \"hookState\" object (having some user-defined
-- abstract type), and this object will be passed to the rest of the hooks as
-- the server handles the process of responding to the HTTP request.
--
-- For example, you could pass a set of hooks to the Snap server that measured
-- timings for each URI handled by the server to produce online statistics and
-- metrics using something like @statsd@ (<https://github.com/etsy/statsd>).



emptyServerConfig :: ServerConfig a
emptyServerConfig = undefined
