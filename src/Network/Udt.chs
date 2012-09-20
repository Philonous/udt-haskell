{-# LANGUAGE ForeignFunctionInterface #-}

#include "udt-wrapper.h"
#include "helpers.h"

module Udt where

import Foreign.Ptr
import Foreign.C.Types

import qualified Network.Socket as Socket
import Network.Socket.Internal

{#context lib="udt" prefix = "udt" #}

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

{# fun udt_startup as ^ {} -> `Int' #}

{# fun udt_socket as socket' {`Int' , `Int', `Int'} -> `CInt' id #}

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

withSockAddr' addr f = withSockAddr addr
                         (\ptr len ->  f (castPtr ptr, fromIntegral len))

{#fun udt_bind as bind' { rawSocket `UdtSocket'
                        , withSockAddr'* `Socket.SockAddr' & } -> `Int'#}


--bind socket addr = do
