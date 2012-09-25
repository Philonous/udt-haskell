{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "udt-wrapper.h"
#include "helpers.h"

module Network.Udt where

import Control.Exception.Base
import Control.Monad(when)

import Data.ByteString(ByteString)
import Data.ByteString.Unsafe(unsafeUseAsCStringLen)
import Data.ByteString.Internal(createAndTrim)

import Data.Typeable

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils(with)
import Foreign.Storable(sizeOf, peek)

import qualified Network.Socket as Socket
import qualified Network.Socket.Internal as Socket

{#context lib="udt" prefix="udt" #}

data UdtSocket = UdtSocket { rawSocket  :: CInt
                           , family     :: Socket.Family
                           , socketType :: Socket.SocketType
                           } deriving (Show, Eq)

{# enum UDTOpt as SockOpt {underscoreToCase} deriving (Eq, Show, Read, Bounded) #}

sockOptList :: [SockOpt]
sockOptList = [minBound .. maxBound ]



-- extractStatusMasks :: (Bits a, Storable a) => Ptr a -> IO [Status]
-- extractStatusMasks = peek >=> \bits ->
--   return [bm | bm <- statusList, bits `containsBitMask` bm]

{# fun pure af_inet  as ^ {} -> `Int' #}
{# fun pure af_inet6 as ^ {} -> `Int' #}
{# fun pure sock_stream as ^ {} -> `Int' #}
{# fun pure sock_dgram  as ^ {} -> `Int' #}
-- {# fun pure invalid_sock as ^ {} -> `CInt' id #}
invalidSock = -1 -- circumvent ghci linkage wonkiness
-- {# fun pure error as errorConst {} -> `Int' #}
errorConst = -1

-- | Initialize the UDT library
{# fun udt_startup as startup {} -> `Int' #}
{# fun udt_cleanup as cleanup {} -> `Int' #}

withUdt = bracket_ startup cleanup

data UdtError = Success      -- ^ success operation.
              | EConnsetup   -- ^ connection setup failure.
              | ENoserver    -- ^ server does not exist.
              | EConnrej     -- ^ connection request was rejected by server.
              | ESockfail    -- ^ could not create/configure UDP socket.
              | ESecfail     -- ^ connection request was aborted due to
                             -- security reasons.
              | EConnfail    -- ^ connection failure.
              | EConnlost    -- ^ connection was broken.
              | ENoConn      -- ^ connection does not exist.
              | EResource    -- ^ system resource failure.
              | EThread      -- ^ could not create new thread.
              | ENobuf       -- ^ no memory space.
              | EFile        -- ^ file access error.
              | EInvRdOff    -- ^ invalid read offset.
              | ERdperm      -- ^ no read permission.
              | EInvWrOff    -- ^ invalid write offset.
              | EWrPerm      -- ^ no write permission.
              | EInvOp       -- ^ operation not supported.
              | EBoundSock   -- ^ cannot execute the operation on a bound
                             -- socket.
              | EConnSock    -- ^ cannot execute the operation on a
                             -- connected socket.
              | EInvParam    -- ^ bad parameters.
              | EInvSock     -- ^ invalid UDT socket.
              | EUnboundSock -- ^ cannot listen on unbound socket.
              | ENoListen    -- ^ (accept) socket is not in listening state.
              | ERdvNoServ   -- ^ rendezvous connection process does not
                             -- allow listen and accept call.
              | ERdvUnbound  -- ^ rendezvous connection setup is enabled but
                             -- bind has not been called before connect.
              | EStreamill   -- ^ operation not supported in SOCK_STREAM mode.
              | EDgramill    -- ^ operation not supported in SOCK_DGRAM mode.
              | EDuplisten   -- ^ another socket is already listening on the
                             -- same UDP port.
              | ELargemsg    -- ^ message is too large to be hold in the
                             -- sending buffer.
              | EAsyncfail   -- ^ non-blocking call failure.
              | EAsyncsnd    -- ^ no buffer available for sending.
              | EAsyncrcv    -- ^ no data available for read.

              | EOther Int   -- ^ An error not listed above
                deriving (Show, Eq, Typeable)


numToErr 0    = Success
numToErr 1000 = EConnsetup
numToErr 1001 = ENoserver
numToErr 1002 = EConnrej
numToErr 1003 = ESockfail
numToErr 1004 = ESecfail
numToErr 2000 = EConnfail
numToErr 2001 = EConnlost
numToErr 2002 = ENoConn
numToErr 3000 = EResource
numToErr 3001 = EThread
numToErr 3002 = ENobuf
numToErr 4000 = EFile
numToErr 4001 = EInvRdOff
numToErr 4002 = ERdperm
numToErr 4003 = EInvWrOff
numToErr 4004 = EWrPerm
numToErr 5000 = EInvOp
numToErr 5001 = EBoundSock
numToErr 5002 = EConnSock
numToErr 5003 = EInvParam
numToErr 5004 = EInvSock
numToErr 5005 = EUnboundSock
numToErr 5006 = ENoListen
numToErr 5007 = ERdvNoServ
numToErr 5008 = ERdvUnbound
numToErr 5009 = EStreamill
numToErr 5010 = EDgramill
numToErr 5011 = EDuplisten
numToErr 5012 = ELargemsg
numToErr 6000 = EAsyncfail
numToErr 6001 = EAsyncsnd
numToErr 6002 = EAsyncrcv
numToErr x    = EOther x

data UdtException = UdtException {errorCode :: UdtError
                                 ,errorDescription :: String}
                    deriving (Show, Eq, Typeable)

instance Exception UdtException

getAndClearError = do
    err <- getLastError
    code <- numToErr `fmap` errorInfoGetErrorCode err
    descr <- errorInfoGetErrorMessage err
    errorInfoClear err
    return $ UdtException code descr

checkErrorInt :: Int -> IO ()
checkErrorInt i = do
    print [i, errorConst]
    when (i == errorConst) $  throw =<< getAndClearError

checkErrorSocket :: CInt -> IO ()
checkErrorSocket s = do
    print [s, invalidSock]
    when (s == invalidSock) $ throw =<< getAndClearError

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
  checkErrorSocket fd
  return $ UdtSocket fd fam tp

withSockAddr' addr f = Socket.withSockAddr addr
                         (\ptr len ->  f (castPtr ptr, fromIntegral len))

{# fun udt_bind as bind' { rawSocket `UdtSocket'
                         , withSockAddr'* `Socket.SockAddr' & } -> `Int'#}

bind :: UdtSocket -> Socket.SockAddr -> IO ()
bind s addr = bind' s addr >>= checkErrorInt

{#fun udt_bind_socket as bindSocket' { rawSocket `UdtSocket'
                        , id `CInt' } -> `Int'#}

bindSocket sock (Socket.MkSocket fd _fa Socket.Datagram _pn _state) =
    bindSocket' sock fd >>= checkErrorInt
bindSocket _ _ = ioError . userError $ "UDT.bindSocket: Socket hast to be of type Datagram"

{#fun udt_listen as listen {rawSocket `UdtSocket', `Int'} -> `Int' #}


accept :: UdtSocket -> IO (UdtSocket, Socket.SockAddr)
accept sock@(UdtSocket raw fam socktp) =
  Socket.withNewSockAddr (fam) $ \addrPtr _bytes ->
  alloca $ \intPtr -> do
      res <- accept'_ raw addrPtr intPtr
      checkErrorSocket res
      addr <- Socket.peekSockAddr addrPtr
      return (UdtSocket res fam socktp, addr)

foreign import ccall unsafe "Network/Udt.chs.h udt_accept"
  accept'_ :: (CInt -> ((Ptr Socket.SockAddr) -> ((Ptr CInt) -> (IO CInt))))

close :: UdtSocket -> IO ()
close s = checkErrorInt =<< close' s
{# fun udt_close as close' {rawSocket `UdtSocket'} -> `Int' #}

connect :: UdtSocket -> Socket.SockAddr -> IO ()
connect sock addr = checkErrorInt =<< connect' sock addr
{# fun udt_connect as connect' { rawSocket     `UdtSocket'
                        , withSockAddr'* `Socket.SockAddr' &} -> `Int' #}

-- connect sock addr = withSockAddr addr $ \(ptr, len) ->


newtype ErrorInfo = ErrorInfo {fromErrorInfo :: Ptr ()}

{#fun udt_getlasterror as getLastError {} -> `ErrorInfo' ErrorInfo #}

{#fun udt_errorinfo_get_error_code as errorInfoGetErrorCode
  {fromErrorInfo `ErrorInfo'} -> `Int' #}

{#fun udt_errorinfo_get_error_message as errorInfoGetErrorMessage
  {fromErrorInfo `ErrorInfo'} -> `String' #}

{#fun udt_errorinfo_clear as errorInfoClear
  {fromErrorInfo `ErrorInfo'} -> `()' #}

getPeerName :: UdtSocket -> IO Socket.SockAddr
getPeerName sock =
  Socket.withNewSockAddr (family sock) $ \addrPtr _bytes ->
  alloca $ \intPtr -> do
      res <- getPeerName'_ (rawSocket sock) addrPtr intPtr
      checkErrorInt $ fromIntegral res
      addr <- Socket.peekSockAddr addrPtr
      return addr

foreign import ccall safe "Network/Udt.chs.h udt_getpeername"
  getPeerName'_ :: (CInt -> ((Ptr Socket.SockAddr) -> ((Ptr CInt) -> (IO CInt))))


unsafeUseAsCStringLen' x f = unsafeUseAsCStringLen x
                             (\(buf, len) -> f (buf, fromIntegral len))


send :: UdtSocket -> ByteString -> IO Int
send sock bs = do
    res <- send' sock bs 0
    checkErrorInt res
    return res

{#fun udt_send as send' { rawSocket `UdtSocket'
                        , unsafeUseAsCStringLen'* `ByteString' &
                        , `Int'
                        }
                        -> `Int' #}

recv :: UdtSocket -> Int -> IO ByteString
recv socket nbytes = do
  createAndTrim nbytes $ \ptr -> do
      res <- fromIntegral `fmap` {#call unsafe udt_recv#} (rawSocket socket)
                                            (castPtr ptr)
                                            (fromIntegral nbytes)
                                            0
      checkErrorInt res
      return res


fromSockOpt = fromIntegral . fromEnum

setSockOpt :: UdtSocket -> SockOpt -> Int -> IO ()
setSockOpt sock opt val = with (fromIntegral val :: CInt) $ \ptr -> do
  res <- setSockOpt' sock 0 opt (castPtr ptr) (fromIntegral (sizeOf (undefined :: CInt)))
  checkErrorInt res

{#fun udt_setsockopt as setSockOpt' { rawSocket `UdtSocket'
                                    , id `CInt'
                                    , fromSockOpt `SockOpt'
                                    , id `Ptr ()'
                                    , `Int'
                                    }
                                    -> `Int' #}


getSocketOption :: UdtSocket
                -> SockOpt  -- Option Name
                -> IO Int        -- Option Value
getSocketOption sock so = do
   alloca $ \(ptr_v :: Ptr CInt) ->
     with (fromIntegral (sizeOf (undefined :: CInt))) $ \(ptr_sz :: Ptr CInt)-> do
         res <- {#call udt_getsockopt#}
                    (rawSocket sock) 0 (fromSockOpt so) (castPtr ptr_v) (castPtr ptr_sz)
         checkErrorInt $ fromIntegral res
         fromIntegral `fmap` peek ptr_v
