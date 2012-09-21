{-# LANGUAGE ForeignFunctionInterface #-}

#include "udt-wrapper.h"
#include "helpers.h"

module Network.Udt where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc

import qualified Network.Socket as Socket
import qualified Network.Socket.Internal as Socket

{#context lib="udt" prefix="udt" #}

data UdtSocket = UdtSocket { rawSocket  :: CInt
                           , family     :: Socket.Family
                           , socketType :: Socket.SocketType
                           } deriving (Show, Eq)

{# enum UDTOpt as UdtOpt {underscoreToCase} deriving (Eq, Show, Read, Bounded)  #}

udtOptList :: [UdtOpt]
udtOptList = [minBound .. maxBound ]

-- extractStatusMasks :: (Bits a, Storable a) => Ptr a -> IO [Status]
-- extractStatusMasks = peek >=> \bits ->
--   return [bm | bm <- statusList, bits `containsBitMask` bm]

{# fun pure af_inet  as ^ {} -> `Int' #}
{# fun pure af_inet6 as ^ {} -> `Int' #}
{# fun pure sock_stream as ^ {} -> `Int' #}
{# fun pure sock_dgram  as ^ {} -> `Int' #}

-- | Initialize the UDT library
{# fun udt_startup as startup {} -> `Int' #}
{# fun udt_cleanup as cleanup {} -> `Int' #}

{# fun udt_socket as socket' {`Int' , `Int', `Int'} -> `CInt' id #}

-- | Create a UDT socket
socket :: Socket.Family  -- ^ AF_INET or AF_INET6
       -> Socket.SocketType -- ^ Stream or Datagram
       -> IO UdtSocket
socket fam tp = do
  af <- case fam of
          Socket.AF_INET -> return afInet
          Socket.AF_INET6 -> return afInet6
          f -> ioError . userError $ "UDT.socket: Family " ++ show f
                                        ++ "not supported."
  t <- case tp of
          Socket.Stream -> return sockStream
          Socket.Datagram -> return sockDgram
          x -> ioError . userError $ "UDT.socket: Socket type " ++ show x
                                        ++ "not supported."
  fd <- socket' af t 0
  return $ UdtSocket fd fam tp

withSockAddr' addr f = Socket.withSockAddr addr
                         (\ptr len ->  f (castPtr ptr, fromIntegral len))

{# fun udt_bind as bind' { rawSocket `UdtSocket'
                        , withSockAddr'* `Socket.SockAddr' & } -> `Int'#}

bind :: UdtSocket -> Socket.SockAddr -> IO Int
bind = bind' -- TODO: check validity of arguments

{#fun udt_bind_socket as bindSocket' { rawSocket `UdtSocket'
                        , id `CInt' } -> `Int'#}


bindSocket sock (Socket.MkSocket fd _fa Socket.Datagram _pn _state) =
    bindSocket' sock fd
bindSocket _ _ = ioError . userError $ "UDT.bindSocket: Socket hast to be of type Datagram"

{#fun udt_listen as listen {rawSocket `UdtSocket', `Int'} -> `Int' #}


accept sock =
  Socket.withNewSockAddr (family sock) $ \addrPtr _bytes ->
  alloca $ \intPtr -> do
      res <- accept'_ (rawSocket sock) addrPtr intPtr
      addr <- Socket.peekSockAddr addrPtr
      return (res, addr)

foreign import ccall safe "Network/Udt.chs.h udt_accept"
  accept'_ :: (CInt -> ((Ptr Socket.SockAddr) -> ((Ptr CInt) -> (IO CInt))))

{# fun udt_close as close {rawSocket `UdtSocket'} -> `Int' #}

{# fun udt_connect as connect { rawSocket     `UdtSocket'
                        , withSockAddr'* `Socket.SockAddr' &} -> `Int' #}

newtype ErrorInfo = ErrorInfo (Ptr ())

{#fun udt_getlasterror as getLastError {} -> `ErrorInfo' ErrorInfo #}