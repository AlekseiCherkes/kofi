{-# INCLUDE "HsNet.h" #-}
{-# OPTIONS_GHC -optc-DDOMAIN_SOCKET_SUPPORT=1 #-}
{-# LINE 1 "Socket.hsc" #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# LINE 2 "Socket.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The "Network.Socket" module is for when you want full control over
-- sockets.  Essentially the entire C socket API is exposed through
-- this module; in general the operations follow the behaviour of the C
-- functions of the same name (consult your favourite Unix networking book).
--
-- A higher level interface to networking operations is provided
-- through the module "Network".
--
-----------------------------------------------------------------------------


{-# LINE 23 "Socket.hsc" #-}

-- NOTE: ##, we want this interpreted when compiling the .hs, not by hsc2hs.
#include "Typeable.h"


{-# LINE 30 "Socket.hsc" #-}


{-# LINE 32 "Socket.hsc" #-}

{-# LINE 33 "Socket.hsc" #-}

{-# LINE 34 "Socket.hsc" #-}


{-# LINE 42 "Socket.hsc" #-}

-- In order to process this file, you need to have CALLCONV defined.

module Network.Socket (

    -- * Types
    Socket(..),		-- instance Eq, Show
    Family(..),		
    SocketType(..),
    SockAddr(..),
    SocketStatus(..),
    HostAddress,

{-# LINE 55 "Socket.hsc" #-}
    HostAddress6,
    FlowInfo,
    ScopeID,

{-# LINE 59 "Socket.hsc" #-}
    ShutdownCmd(..),
    ProtocolNumber,
    defaultProtocol,        -- :: ProtocolNumber
    PortNumber(..),
	-- PortNumber is used non-abstractly in Network.BSD.  ToDo: remove
	-- this use and make the type abstract.

    -- * Address operations

    HostName,
    ServiceName,


{-# LINE 72 "Socket.hsc" #-}
    AddrInfo(..),

    AddrInfoFlag(..),
    addrInfoFlagImplemented,-- :: AddrInfoFlag -> Bool

    defaultHints,           -- :: AddrInfo

    getAddrInfo,            -- :: Maybe AddrInfo -> Maybe HostName -> Maybe ServiceName -> IO [AddrInfo]

    NameInfoFlag(..),

    getNameInfo,            -- :: [NameInfoFlag] -> Bool -> Bool -> SockAddr -> IO (Maybe HostName, Maybe ServiceName)

{-# LINE 85 "Socket.hsc" #-}

    -- * Socket Operations
    socket,		-- :: Family -> SocketType -> ProtocolNumber -> IO Socket 

{-# LINE 89 "Socket.hsc" #-}
    socketPair,         -- :: Family -> SocketType -> ProtocolNumber -> IO (Socket, Socket)

{-# LINE 91 "Socket.hsc" #-}
    connect,		-- :: Socket -> SockAddr -> IO ()
    bindSocket,		-- :: Socket -> SockAddr -> IO ()
    listen,		-- :: Socket -> Int -> IO ()
    accept,		-- :: Socket -> IO (Socket, SockAddr)
    getPeerName,	-- :: Socket -> IO SockAddr
    getSocketName,	-- :: Socket -> IO SockAddr


{-# LINE 102 "Socket.hsc" #-}

    socketPort,		-- :: Socket -> IO PortNumber

    socketToHandle,	-- :: Socket -> IOMode -> IO Handle

    sendTo,		-- :: Socket -> String -> SockAddr -> IO Int
    sendBufTo,          -- :: Socket -> Ptr a -> Int -> SockAddr -> IO Int

    recvFrom,		-- :: Socket -> Int -> IO (String, Int, SockAddr)
    recvBufFrom,        -- :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
    
    send,		-- :: Socket -> String -> IO Int
    recv,		-- :: Socket -> Int    -> IO String
    recvLen,            -- :: Socket -> Int    -> IO (String, Int)

    inet_addr,		-- :: String -> IO HostAddress
    inet_ntoa,		-- :: HostAddress -> IO String

    shutdown,		-- :: Socket -> ShutdownCmd -> IO ()
    sClose,		-- :: Socket -> IO ()

    -- ** Predicates on sockets
    sIsConnected,	-- :: Socket -> IO Bool
    sIsBound,		-- :: Socket -> IO Bool
    sIsListening,	-- :: Socket -> IO Bool 
    sIsReadable,	-- :: Socket -> IO Bool
    sIsWritable,	-- :: Socket -> IO Bool

    -- * Socket options
    SocketOption(..),
    getSocketOption,     -- :: Socket -> SocketOption -> IO Int
    setSocketOption,     -- :: Socket -> SocketOption -> Int -> IO ()

    -- * File descriptor transmission

{-# LINE 137 "Socket.hsc" #-}
    sendFd,              -- :: Socket -> CInt -> IO ()
    recvFd,              -- :: Socket -> IO CInt

      -- Note: these two will disappear shortly
    sendAncillary,       -- :: Socket -> Int -> Int -> Int -> Ptr a -> Int -> IO ()
    recvAncillary,       -- :: Socket -> Int -> Int -> IO (Int,Int,Int,Ptr a)


{-# LINE 145 "Socket.hsc" #-}

    -- * Special Constants
    aNY_PORT,		-- :: PortNumber
    iNADDR_ANY,		-- :: HostAddress

{-# LINE 150 "Socket.hsc" #-}
    iN6ADDR_ANY,	-- :: HostAddress6

{-# LINE 152 "Socket.hsc" #-}
    sOMAXCONN,		-- :: Int
    sOL_SOCKET,         -- :: Int

{-# LINE 155 "Socket.hsc" #-}
    sCM_RIGHTS,         -- :: Int

{-# LINE 157 "Socket.hsc" #-}
    maxListenQueue,	-- :: Int

    -- * Initialisation
    withSocketsDo,	-- :: IO a -> IO a
    
    -- * Very low level operations
     -- in case you ever want to get at the underlying file descriptor..
    fdSocket,           -- :: Socket -> CInt
    mkSocket,           -- :: CInt   -> Family 
    			-- -> SocketType
			-- -> ProtocolNumber
			-- -> SocketStatus
			-- -> IO Socket

    -- * Internal

    -- | The following are exported ONLY for use in the BSD module and
    -- should not be used anywhere else.

    packFamily, unpackFamily,
    packSocketType,
    throwSocketErrorIfMinus1_

) where


{-# LINE 194 "Socket.hsc" #-}

import Data.Bits
import Data.List (foldl')
import Data.Word ( Word8, Word16, Word32 )
import Foreign.Ptr ( Ptr, castPtr, nullPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import Foreign.C.Error
import Foreign.C.String ( CString, withCString, peekCString, peekCStringLen, castCharToCChar )
import Foreign.C.Types ( CInt, CUInt, CChar, CSize )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Array ( peekArray, pokeArray, pokeArray0 )
import Foreign.Marshal.Utils ( maybeWith, with )

import System.IO
import Control.Monad ( liftM, when )
import Data.Ratio ( (%) )

import qualified Control.Exception
import Control.Concurrent.MVar
import Data.Typeable
import System.IO.Error


{-# LINE 217 "Socket.hsc" #-}
import GHC.Conc		(threadWaitRead, threadWaitWrite)

{-# LINE 222 "Socket.hsc" #-}
import GHC.Handle
import GHC.IOBase
import qualified System.Posix.Internals

{-# LINE 228 "Socket.hsc" #-}

import Network.Socket.Internal

type HostName       = String
type ServiceName    = String

-- ----------------------------------------------------------------------------
-- On Windows, our sockets are not put in non-blocking mode (non-blocking
-- is not supported for regular file descriptors on Windows, and it would
-- be a pain to support it only for sockets).  So there are two cases:
--
--  - the threaded RTS uses safe calls for socket operations to get
--    non-blocking I/O, just like the rest of the I/O library
--
--  - with the non-threaded RTS, only some operations on sockets will be
--    non-blocking.  Reads and writes go through the normal async I/O
--    system.  accept() uses asyncDoProc so is non-blocking.  A handful
--    of others (recvFrom, sendFd, recvFd) will block all threads - if this
--    is a problem, -threaded is the workaround.
--
#if defined(mingw32_HOST_OS)
#define SAFE_ON_WIN safe
#else
#define SAFE_ON_WIN unsafe
#endif

-----------------------------------------------------------------------------
-- Socket types

-- There are a few possible ways to do this.  The first is convert the
-- structs used in the C library into an equivalent Haskell type. An
-- other possible implementation is to keep all the internals in the C
-- code and use an Int## and a status flag. The second method is used
-- here since a lot of the C structures are not required to be
-- manipulated.

-- Originally the status was non-mutable so we had to return a new
-- socket each time we changed the status.  This version now uses
-- mutable variables to avoid the need to do this.  The result is a
-- cleaner interface and better security since the application
-- programmer now can't circumvent the status information to perform
-- invalid operations on sockets.

data SocketStatus
  -- Returned Status	Function called
  = NotConnected	-- socket
  | Bound		-- bindSocket
  | Listening		-- listen
  | Connected		-- connect/accept
  | ConvertedToHandle   -- is now a Handle, don't touch
  | Closed		-- sClose 
    deriving (Eq, Show)

INSTANCE_TYPEABLE0(SocketStatus,socketStatusTc,"SocketStatus")

data Socket
  = MkSocket
	    CInt	         -- File Descriptor
	    Family				  
	    SocketType				  
	    ProtocolNumber	 -- Protocol Number
	    (MVar SocketStatus)  -- Status Flag

INSTANCE_TYPEABLE0(Socket,socketTc,"Socket")

mkSocket :: CInt
	 -> Family
	 -> SocketType
	 -> ProtocolNumber
	 -> SocketStatus
	 -> IO Socket
mkSocket fd fam sType pNum stat = do
   mStat <- newMVar stat
   return (MkSocket fd fam sType pNum mStat)

instance Eq Socket where
  (MkSocket _ _ _ _ m1) == (MkSocket _ _ _ _ m2) = m1 == m2

instance Show Socket where
  showsPrec n (MkSocket fd _ _ _ _) = 
	showString "<socket: " . shows fd . showString ">"


fdSocket :: Socket -> CInt
fdSocket (MkSocket fd _ _ _ _) = fd

type ProtocolNumber = CInt

-- | This is the default protocol for a given service.
defaultProtocol :: ProtocolNumber
defaultProtocol = 0

----------------------------------------------------------------------------
-- Port Numbers

INSTANCE_TYPEABLE0(PortNumber,portNumberTc,"PortNumber")

instance Show PortNumber where
  showsPrec p pn = showsPrec p (portNumberToInt pn)

intToPortNumber :: Int -> PortNumber
intToPortNumber v = PortNum (htons (fromIntegral v))

portNumberToInt :: PortNumber -> Int
portNumberToInt (PortNum po) = fromIntegral (ntohs po)

foreign import CALLCONV unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import CALLCONV unsafe "htons" htons :: Word16 -> Word16
--foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32

instance Enum PortNumber where
    toEnum   = intToPortNumber
    fromEnum = portNumberToInt

instance Num PortNumber where
   fromInteger i = intToPortNumber (fromInteger i)
    -- for completeness.
   (+) x y   = intToPortNumber (portNumberToInt x + portNumberToInt y)
   (-) x y   = intToPortNumber (portNumberToInt x - portNumberToInt y)
   negate x  = intToPortNumber (-portNumberToInt x)
   (*) x y   = intToPortNumber (portNumberToInt x * portNumberToInt y)
   abs n     = intToPortNumber (abs (portNumberToInt n))
   signum n  = intToPortNumber (signum (portNumberToInt n))

instance Real PortNumber where
    toRational x = toInteger x % 1

instance Integral PortNumber where
    quotRem a b = let (c,d) = quotRem (portNumberToInt a) (portNumberToInt b) in
		  (intToPortNumber c, intToPortNumber d)
    toInteger a = toInteger (portNumberToInt a)

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (undefined :: Word16)
   alignment _ = alignment (undefined :: Word16)
   poke p (PortNum po) = poke (castPtr p) po
   peek p = PortNum `liftM` peek (castPtr p)

-----------------------------------------------------------------------------
-- SockAddr

INSTANCE_TYPEABLE0(SockAddr,sockAddrTc,"SockAddr")

instance Show SockAddr where

{-# LINE 374 "Socket.hsc" #-}
  showsPrec _ (SockAddrUnix str) = showString str

{-# LINE 376 "Socket.hsc" #-}
  showsPrec _ (SockAddrInet port ha)
   = showString (unsafePerformIO (inet_ntoa ha)) 
   . showString ":"
   . shows port

{-# LINE 381 "Socket.hsc" #-}
  showsPrec _ addr@(SockAddrInet6 port _ _ _)
   = showChar '['
   . showString (unsafePerformIO $
                 fst `liftM` getNameInfo [NI_NUMERICHOST] True False addr >>=
                 maybe (fail "showsPrec: impossible internal error") return)
   . showString "]:"
   . shows port

{-# LINE 389 "Socket.hsc" #-}

-----------------------------------------------------------------------------
-- Connection Functions

-- In the following connection and binding primitives.  The names of
-- the equivalent C functions have been preserved where possible. It
-- should be noted that some of these names used in the C library,
-- \tr{bind} in particular, have a different meaning to many Haskell
-- programmers and have thus been renamed by appending the prefix
-- Socket.

-- Create an unconnected socket of the given family, type and
-- protocol.  The most common invocation of $socket$ is the following:
--    ...
--    my_socket <- socket AF_INET Stream 6
--    ...

socket :: Family 	 -- Family Name (usually AF_INET)
       -> SocketType 	 -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket	 -- Unconnected Socket

socket family stype protocol = do
    fd <- throwSocketErrorIfMinus1Retry "socket" $
		c_socket (packFamily family) (packSocketType stype) protocol

{-# LINE 415 "Socket.hsc" #-}
    System.Posix.Internals.setNonBlockingFD fd

{-# LINE 417 "Socket.hsc" #-}
    socket_status <- newMVar NotConnected
    return (MkSocket fd family stype protocol socket_status)

-- Create an unnamed pair of connected sockets, given family, type and
-- protocol. Differs from a normal pipe in being a bi-directional channel
-- of communication.


{-# LINE 425 "Socket.hsc" #-}
socketPair :: Family 	          -- Family Name (usually AF_INET or AF_INET6)
           -> SocketType 	  -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.
socketPair family stype protocol = do
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
    rc <- throwSocketErrorIfMinus1Retry "socketpair" $
		c_socketpair (packFamily family)
			     (packSocketType stype)
			     protocol fdArr
    [fd1,fd2] <- peekArray 2 fdArr 
    s1 <- mkSocket fd1
    s2 <- mkSocket fd2
    return (s1,s2)
  where
    mkSocket fd = do

{-# LINE 442 "Socket.hsc" #-}
       System.Posix.Internals.setNonBlockingFD fd

{-# LINE 444 "Socket.hsc" #-}
       stat <- newMVar Connected
       return (MkSocket fd family stype protocol stat)

foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

{-# LINE 450 "Socket.hsc" #-}

-----------------------------------------------------------------------------
-- Binding a socket
--
-- Given a port number this {\em binds} the socket to that port. This
-- means that the programmer is only interested in data being sent to
-- that port number. The $Family$ passed to $bindSocket$ must
-- be the same as that passed to $socket$.	 If the special port
-- number $aNY\_PORT$ is passed then the system assigns the next
-- available use port.
-- 
-- Port numbers for standard unix services can be found by calling
-- $getServiceEntry$.  These are traditionally port numbers below
-- 1000; although there are afew, namely NFS and IRC, which used higher
-- numbered ports.
-- 
-- The port number allocated to a socket bound by using $aNY\_PORT$ can be
-- found by calling $port$

bindSocket :: Socket	-- Unconnected Socket
	   -> SockAddr	-- Address to Bind to
	   -> IO ()

bindSocket (MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \ status -> do
 if status /= NotConnected 
  then
   ioError (userError ("bindSocket: can't peform bind on socket in status " ++
	 show status))
  else do
   withSockAddr addr $ \p_addr sz -> do
   status <- throwSocketErrorIfMinus1Retry "bind" $ c_bind s p_addr (fromIntegral sz)
   return Bound

-----------------------------------------------------------------------------
-- Connecting a socket
--
-- Make a connection to an already opened socket on a given machine
-- and port.  assumes that we have already called createSocket,
-- otherwise it will fail.
--
-- This is the dual to $bindSocket$.  The {\em server} process will
-- usually bind to a port number, the {\em client} will then connect
-- to the same port number.  Port numbers of user applications are
-- normally agreed in advance, otherwise we must rely on some meta
-- protocol for telling the other side what port number we have been
-- allocated.

connect :: Socket	-- Unconnected Socket
	-> SockAddr 	-- Socket address stuff
	-> IO ()

connect sock@(MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \currentStatus -> do
 if currentStatus /= NotConnected 
  then
   ioError (userError ("connect: can't peform connect on socket in status " ++
         show currentStatus))
  else do
   withSockAddr addr $ \p_addr sz -> do

   let  connectLoop = do
       	   r <- c_connect s p_addr (fromIntegral sz)
       	   if r == -1
       	       then do 

{-# LINE 516 "Socket.hsc" #-}
	       	       err <- getErrno
		       case () of
			 _ | err == eINTR       -> connectLoop
			 _ | err == eINPROGRESS -> connectBlocked
--			 _ | err == eAGAIN      -> connectBlocked
			 otherwise              -> throwSocketError "connect"

{-# LINE 533 "Socket.hsc" #-}
       	       else return r

	connectBlocked = do 

{-# LINE 537 "Socket.hsc" #-}
	   threadWaitWrite (fromIntegral s)

{-# LINE 539 "Socket.hsc" #-}
	   err <- getSocketOption sock SoError
	   if (err == 0)
	   	then return 0
	   	else do ioError (errnoToIOError "connect" 
	   			(Errno (fromIntegral err))
	   			Nothing Nothing)

   connectLoop
   return Connected

-----------------------------------------------------------------------------
-- Listen
--
-- The programmer must call $listen$ to tell the system software that
-- they are now interested in receiving data on this port.  This must
-- be called on the bound socket before any calls to read or write
-- data are made.

-- The programmer also gives a number which indicates the length of
-- the incoming queue of unread messages for this socket. On most
-- systems the maximum queue length is around 5.  To remove a message
-- from the queue for processing a call to $accept$ should be made.

listen :: Socket  -- Connected & Bound Socket
       -> Int 	  -- Queue Length
       -> IO ()

listen (MkSocket s _family _stype _protocol socketStatus) backlog = do
 modifyMVar_ socketStatus $ \ status -> do
 if status /= Bound 
   then
    ioError (userError ("listen: can't peform listen on socket in status " ++
          show status))
   else do
    throwSocketErrorIfMinus1Retry "listen" (c_listen s (fromIntegral backlog))
    return Listening

-----------------------------------------------------------------------------
-- Accept
--
-- A call to `accept' only returns when data is available on the given
-- socket, unless the socket has been set to non-blocking.  It will
-- return a new socket which should be used to read the incoming data and
-- should then be closed. Using the socket returned by `accept' allows
-- incoming requests to be queued on the original socket.

accept :: Socket			-- Queue Socket
       -> IO (Socket,			-- Readable Socket
	      SockAddr)			-- Peer details

accept sock@(MkSocket s family stype protocol status) = do
 currentStatus <- readMVar status
 okay <- sIsAcceptable sock
 if not okay
   then
     ioError (userError ("accept: can't perform accept on socket (" ++ (show (family,stype,protocol)) ++") in status " ++
	 show currentStatus))
   else do
     let sz = sizeOfSockAddrByFamily family
     allocaBytes sz $ \ sockaddr -> do

{-# LINE 614 "Socket.hsc" #-}
     with (fromIntegral sz) $ \ ptr_len -> do
     new_sock <- 

{-# LINE 617 "Socket.hsc" #-}
                 throwSocketErrorIfMinus1RetryMayBlock "accept"
			(threadWaitRead (fromIntegral s))

{-# LINE 620 "Socket.hsc" #-}
			(c_accept s sockaddr ptr_len)

{-# LINE 622 "Socket.hsc" #-}
     System.Posix.Internals.setNonBlockingFD new_sock

{-# LINE 624 "Socket.hsc" #-}

{-# LINE 625 "Socket.hsc" #-}
     addr <- peekSockAddr sockaddr
     new_status <- newMVar Connected
     return ((MkSocket new_sock family stype protocol new_status), addr)


{-# LINE 639 "Socket.hsc" #-}

-----------------------------------------------------------------------------
-- sendTo & recvFrom

-- | NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
sendTo :: Socket	-- (possibly) bound/connected Socket
       -> String	-- Data to send
       -> SockAddr
       -> IO Int	-- Number of Bytes sent

sendTo sock xs addr = do
 withCString xs $ \str -> do
   sendBufTo sock str (length xs) addr

sendBufTo :: Socket	      -- (possibly) bound/connected Socket
          -> Ptr a -> Int     -- Data to send
          -> SockAddr
          -> IO Int	      -- Number of Bytes sent

sendBufTo (MkSocket s _family _stype _protocol status) ptr nbytes addr = do
 withSockAddr addr $ \p_addr sz -> do
   liftM fromIntegral $

{-# LINE 663 "Socket.hsc" #-}
     throwSocketErrorIfMinus1RetryMayBlock "sendTo"
	(threadWaitWrite (fromIntegral s)) $

{-# LINE 666 "Socket.hsc" #-}
	c_sendto s ptr (fromIntegral $ nbytes) 0{-flags-} 
			p_addr (fromIntegral sz)

-- | NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom sock nbytes =
  allocaBytes nbytes $ \ptr -> do
    (len, sockaddr) <- recvBufFrom sock ptr nbytes
    str <- peekCStringLen (ptr, len)
    return (str, len, sockaddr)

recvBufFrom :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
recvBufFrom sock@(MkSocket s family _stype _protocol status) ptr nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvFrom")
 | otherwise   = 
    withNewSockAddr family $ \ptr_addr sz -> do
      alloca $ \ptr_len -> do
      	poke ptr_len (fromIntegral sz)
        len <- 

{-# LINE 687 "Socket.hsc" #-}
	       throwSocketErrorIfMinus1RetryMayBlock "recvFrom"
        	   (threadWaitRead (fromIntegral s)) $

{-# LINE 690 "Socket.hsc" #-}
        	   c_recvfrom s ptr (fromIntegral nbytes) 0{-flags-} 
				ptr_addr ptr_len
        let len' = fromIntegral len
	if len' == 0
	 then ioError (mkEOFError "Network.Socket.recvFrom")
	 else do
   	   flg <- sIsConnected sock
	     -- For at least one implementation (WinSock 2), recvfrom() ignores
	     -- filling in the sockaddr for connected TCP sockets. Cope with 
	     -- this by using getPeerName instead.
	   sockaddr <- 
		if flg then
		   getPeerName sock
		else
		   peekSockAddr ptr_addr 
           return (len', sockaddr)

-----------------------------------------------------------------------------
-- send & recv

send :: Socket	-- Bound/Connected Socket
     -> String	-- Data to send
     -> IO Int	-- Number of Bytes sent
send (MkSocket s _family _stype _protocol status) xs = do
 let len = length xs
 withCString xs $ \str -> do
   liftM fromIntegral $

{-# LINE 721 "Socket.hsc" #-}

{-# LINE 722 "Socket.hsc" #-}
     throwSocketErrorIfMinus1RetryMayBlock "send"
	(threadWaitWrite (fromIntegral s)) $

{-# LINE 725 "Socket.hsc" #-}
	c_send s str (fromIntegral len) 0{-flags-} 

{-# LINE 727 "Socket.hsc" #-}

recv :: Socket -> Int -> IO String
recv sock l = recvLen sock l >>= \ (s,_) -> return s

recvLen :: Socket -> Int -> IO (String, Int)
recvLen sock@(MkSocket s _family _stype _protocol status) nbytes 
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recv")
 | otherwise   = do
     allocaBytes nbytes $ \ptr -> do
        len <- 

{-# LINE 741 "Socket.hsc" #-}

{-# LINE 742 "Socket.hsc" #-}
	       throwSocketErrorIfMinus1RetryMayBlock "recv"
        	   (threadWaitRead (fromIntegral s)) $

{-# LINE 745 "Socket.hsc" #-}
        	   c_recv s ptr (fromIntegral nbytes) 0{-flags-} 

{-# LINE 747 "Socket.hsc" #-}
        let len' = fromIntegral len
	if len' == 0
	 then ioError (mkEOFError "Network.Socket.recv")
	 else do
	   s <- peekCStringLen (ptr,len')
	   return (s, len')

-- ---------------------------------------------------------------------------
-- socketPort
--
-- The port number the given socket is currently connected to can be
-- determined by calling $port$, is generally only useful when bind
-- was given $aNY\_PORT$.

socketPort :: Socket		-- Connected & Bound Socket
	   -> IO PortNumber	-- Port Number of Socket
socketPort sock@(MkSocket _ AF_INET _ _ _) = do
    (SockAddrInet port _) <- getSocketName sock
    return port

{-# LINE 767 "Socket.hsc" #-}
socketPort sock@(MkSocket _ AF_INET6 _ _ _) = do
    (SockAddrInet6 port _ _ _) <- getSocketName sock
    return port

{-# LINE 771 "Socket.hsc" #-}
socketPort (MkSocket _ family _ _ _) =
    ioError (userError ("socketPort: not supported for Family " ++ show family))


-- ---------------------------------------------------------------------------
-- getPeerName

-- Calling $getPeerName$ returns the address details of the machine,
-- other than the local one, which is connected to the socket. This is
-- used in programs such as FTP to determine where to send the
-- returning data.  The corresponding call to get the details of the
-- local machine is $getSocketName$.

getPeerName   :: Socket -> IO SockAddr
getPeerName (MkSocket s family _ _ _) = do
 withNewSockAddr family $ \ptr sz -> do
   with (fromIntegral sz) $ \int_star -> do
   throwSocketErrorIfMinus1Retry "getPeerName" $ c_getpeername s ptr int_star
   sz <- peek int_star
   peekSockAddr ptr
    
getSocketName :: Socket -> IO SockAddr
getSocketName (MkSocket s family _ _ _) = do
 withNewSockAddr family $ \ptr sz -> do
   with (fromIntegral sz) $ \int_star -> do
   throwSocketErrorIfMinus1Retry "getSocketName" $ c_getsockname s ptr int_star
   peekSockAddr ptr

-----------------------------------------------------------------------------
-- Socket Properties

data SocketOption
    = DummySocketOption__

{-# LINE 805 "Socket.hsc" #-}
    | Debug         {- SO_DEBUG     -}

{-# LINE 807 "Socket.hsc" #-}

{-# LINE 808 "Socket.hsc" #-}
    | ReuseAddr     {- SO_REUSEADDR -}

{-# LINE 810 "Socket.hsc" #-}

{-# LINE 811 "Socket.hsc" #-}
    | Type          {- SO_TYPE      -}

{-# LINE 813 "Socket.hsc" #-}

{-# LINE 814 "Socket.hsc" #-}
    | SoError       {- SO_ERROR     -}

{-# LINE 816 "Socket.hsc" #-}

{-# LINE 817 "Socket.hsc" #-}
    | DontRoute     {- SO_DONTROUTE -}

{-# LINE 819 "Socket.hsc" #-}

{-# LINE 820 "Socket.hsc" #-}
    | Broadcast     {- SO_BROADCAST -}

{-# LINE 822 "Socket.hsc" #-}

{-# LINE 823 "Socket.hsc" #-}
    | SendBuffer    {- SO_SNDBUF    -}

{-# LINE 825 "Socket.hsc" #-}

{-# LINE 826 "Socket.hsc" #-}
    | RecvBuffer    {- SO_RCVBUF    -}

{-# LINE 828 "Socket.hsc" #-}

{-# LINE 829 "Socket.hsc" #-}
    | KeepAlive     {- SO_KEEPALIVE -}

{-# LINE 831 "Socket.hsc" #-}

{-# LINE 832 "Socket.hsc" #-}
    | OOBInline     {- SO_OOBINLINE -}

{-# LINE 834 "Socket.hsc" #-}

{-# LINE 835 "Socket.hsc" #-}
    | TimeToLive    {- IP_TTL       -}

{-# LINE 837 "Socket.hsc" #-}

{-# LINE 838 "Socket.hsc" #-}
    | MaxSegment    {- TCP_MAXSEG   -}

{-# LINE 840 "Socket.hsc" #-}

{-# LINE 841 "Socket.hsc" #-}
    | NoDelay       {- TCP_NODELAY  -}

{-# LINE 843 "Socket.hsc" #-}

{-# LINE 844 "Socket.hsc" #-}
    | Linger        {- SO_LINGER    -}

{-# LINE 846 "Socket.hsc" #-}

{-# LINE 849 "Socket.hsc" #-}

{-# LINE 850 "Socket.hsc" #-}
    | RecvLowWater  {- SO_RCVLOWAT  -}

{-# LINE 852 "Socket.hsc" #-}

{-# LINE 853 "Socket.hsc" #-}
    | SendLowWater  {- SO_SNDLOWAT  -}

{-# LINE 855 "Socket.hsc" #-}

{-# LINE 856 "Socket.hsc" #-}
    | RecvTimeOut   {- SO_RCVTIMEO  -}

{-# LINE 858 "Socket.hsc" #-}

{-# LINE 859 "Socket.hsc" #-}
    | SendTimeOut   {- SO_SNDTIMEO  -}

{-# LINE 861 "Socket.hsc" #-}

{-# LINE 864 "Socket.hsc" #-}

INSTANCE_TYPEABLE0(SocketOption,socketOptionTc,"SocketOption")

socketOptLevel :: SocketOption -> CInt
socketOptLevel so = 
  case so of

{-# LINE 871 "Socket.hsc" #-}
    TimeToLive   -> 0
{-# LINE 872 "Socket.hsc" #-}

{-# LINE 873 "Socket.hsc" #-}

{-# LINE 874 "Socket.hsc" #-}
    MaxSegment   -> 6
{-# LINE 875 "Socket.hsc" #-}

{-# LINE 876 "Socket.hsc" #-}

{-# LINE 877 "Socket.hsc" #-}
    NoDelay      -> 6
{-# LINE 878 "Socket.hsc" #-}

{-# LINE 879 "Socket.hsc" #-}
    _            -> 1
{-# LINE 880 "Socket.hsc" #-}

packSocketOption :: SocketOption -> CInt
packSocketOption so =
  case so of

{-# LINE 885 "Socket.hsc" #-}
    Debug         -> 1
{-# LINE 886 "Socket.hsc" #-}

{-# LINE 887 "Socket.hsc" #-}

{-# LINE 888 "Socket.hsc" #-}
    ReuseAddr     -> 2
{-# LINE 889 "Socket.hsc" #-}

{-# LINE 890 "Socket.hsc" #-}

{-# LINE 891 "Socket.hsc" #-}
    Type          -> 3
{-# LINE 892 "Socket.hsc" #-}

{-# LINE 893 "Socket.hsc" #-}

{-# LINE 894 "Socket.hsc" #-}
    SoError       -> 4
{-# LINE 895 "Socket.hsc" #-}

{-# LINE 896 "Socket.hsc" #-}

{-# LINE 897 "Socket.hsc" #-}
    DontRoute     -> 5
{-# LINE 898 "Socket.hsc" #-}

{-# LINE 899 "Socket.hsc" #-}

{-# LINE 900 "Socket.hsc" #-}
    Broadcast     -> 6
{-# LINE 901 "Socket.hsc" #-}

{-# LINE 902 "Socket.hsc" #-}

{-# LINE 903 "Socket.hsc" #-}
    SendBuffer    -> 7
{-# LINE 904 "Socket.hsc" #-}

{-# LINE 905 "Socket.hsc" #-}

{-# LINE 906 "Socket.hsc" #-}
    RecvBuffer    -> 8
{-# LINE 907 "Socket.hsc" #-}

{-# LINE 908 "Socket.hsc" #-}

{-# LINE 909 "Socket.hsc" #-}
    KeepAlive     -> 9
{-# LINE 910 "Socket.hsc" #-}

{-# LINE 911 "Socket.hsc" #-}

{-# LINE 912 "Socket.hsc" #-}
    OOBInline     -> 10
{-# LINE 913 "Socket.hsc" #-}

{-# LINE 914 "Socket.hsc" #-}

{-# LINE 915 "Socket.hsc" #-}
    TimeToLive    -> 2
{-# LINE 916 "Socket.hsc" #-}

{-# LINE 917 "Socket.hsc" #-}

{-# LINE 918 "Socket.hsc" #-}
    MaxSegment    -> 2
{-# LINE 919 "Socket.hsc" #-}

{-# LINE 920 "Socket.hsc" #-}

{-# LINE 921 "Socket.hsc" #-}
    NoDelay       -> 1
{-# LINE 922 "Socket.hsc" #-}

{-# LINE 923 "Socket.hsc" #-}

{-# LINE 924 "Socket.hsc" #-}
    Linger	  -> 13
{-# LINE 925 "Socket.hsc" #-}

{-# LINE 926 "Socket.hsc" #-}

{-# LINE 929 "Socket.hsc" #-}

{-# LINE 930 "Socket.hsc" #-}
    RecvLowWater  -> 18
{-# LINE 931 "Socket.hsc" #-}

{-# LINE 932 "Socket.hsc" #-}

{-# LINE 933 "Socket.hsc" #-}
    SendLowWater  -> 19
{-# LINE 934 "Socket.hsc" #-}

{-# LINE 935 "Socket.hsc" #-}

{-# LINE 936 "Socket.hsc" #-}
    RecvTimeOut   -> 20
{-# LINE 937 "Socket.hsc" #-}

{-# LINE 938 "Socket.hsc" #-}

{-# LINE 939 "Socket.hsc" #-}
    SendTimeOut   -> 21
{-# LINE 940 "Socket.hsc" #-}

{-# LINE 941 "Socket.hsc" #-}

{-# LINE 944 "Socket.hsc" #-}

setSocketOption :: Socket 
		-> SocketOption -- Option Name
		-> Int		-- Option Value
		-> IO ()
setSocketOption (MkSocket s _ _ _ _) so v = do
   with (fromIntegral v) $ \ptr_v -> do
   throwErrnoIfMinus1_ "setSocketOption" $
       c_setsockopt s (socketOptLevel so) (packSocketOption so) ptr_v 
	  (fromIntegral (sizeOf v))
   return ()


getSocketOption :: Socket
		-> SocketOption  -- Option Name
		-> IO Int	 -- Option Value
getSocketOption (MkSocket s _ _ _ _) so = do
   alloca $ \ptr_v ->
     with (fromIntegral (sizeOf (undefined :: CInt))) $ \ptr_sz -> do
       throwErrnoIfMinus1 "getSocketOption" $
	 c_getsockopt s (socketOptLevel so) (packSocketOption so) ptr_v ptr_sz
       fromIntegral `liftM` peek ptr_v



{-# LINE 986 "Socket.hsc" #-}


{-# LINE 988 "Socket.hsc" #-}
-- sending/receiving ancillary socket data; low-level mechanism
-- for transmitting file descriptors, mainly.
sendFd :: Socket -> CInt -> IO ()
sendFd sock outfd = do
  let fd = fdSocket sock

{-# LINE 994 "Socket.hsc" #-}
  throwSocketErrorIfMinus1RetryMayBlock "sendFd"
     (threadWaitWrite (fromIntegral fd)) $
     c_sendFd fd outfd

{-# LINE 1000 "Socket.hsc" #-}
   -- Note: If Winsock supported FD-passing, thi would have been 
   -- incorrect (since socket FDs need to be closed via closesocket().)
  c_close outfd
  return ()
  
recvFd :: Socket -> IO CInt
recvFd sock = do
  let fd = fdSocket sock
  theFd <- 

{-# LINE 1010 "Socket.hsc" #-}
    throwSocketErrorIfMinus1RetryMayBlock "recvFd" 
        (threadWaitRead (fromIntegral fd)) $

{-# LINE 1013 "Socket.hsc" #-}
         c_recvFd fd
  return theFd


sendAncillary :: Socket
	      -> Int
	      -> Int
	      -> Int
	      -> Ptr a
	      -> Int
	      -> IO ()
sendAncillary sock level ty flags datum len = do
  let fd = fdSocket sock
  _ <-

{-# LINE 1028 "Socket.hsc" #-}
   throwSocketErrorIfMinus1RetryMayBlock "sendAncillary"
     (threadWaitWrite (fromIntegral fd)) $

{-# LINE 1031 "Socket.hsc" #-}
     c_sendAncillary fd (fromIntegral level) (fromIntegral ty)
     			(fromIntegral flags) datum (fromIntegral len)
  return ()

recvAncillary :: Socket
	      -> Int
	      -> Int
	      -> IO (Int,Int,Ptr a,Int)
recvAncillary sock flags len = do
  let fd = fdSocket sock
  alloca      $ \ ptr_len   ->
   alloca      $ \ ptr_lev   ->
    alloca      $ \ ptr_ty    ->
     alloca      $ \ ptr_pData -> do
      poke ptr_len (fromIntegral len)
      _ <- 

{-# LINE 1048 "Socket.hsc" #-}
        throwSocketErrorIfMinus1RetryMayBlock "recvAncillary" 
            (threadWaitRead (fromIntegral fd)) $

{-# LINE 1051 "Socket.hsc" #-}
	    c_recvAncillary fd ptr_lev ptr_ty (fromIntegral flags) ptr_pData ptr_len
      len <- fromIntegral `liftM` peek ptr_len
      lev <- fromIntegral `liftM` peek ptr_lev
      ty  <- fromIntegral `liftM` peek ptr_ty
      pD  <- peek ptr_pData
      return (lev,ty,pD, len)
foreign import ccall SAFE_ON_WIN "sendAncillary"
  c_sendAncillary :: CInt -> CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

foreign import ccall SAFE_ON_WIN "recvAncillary"
  c_recvAncillary :: CInt -> Ptr CInt -> Ptr CInt -> CInt -> Ptr (Ptr a) -> Ptr CInt -> IO CInt

foreign import ccall SAFE_ON_WIN "sendFd" c_sendFd :: CInt -> CInt -> IO CInt
foreign import ccall SAFE_ON_WIN "recvFd" c_recvFd :: CInt -> IO CInt


{-# LINE 1067 "Socket.hsc" #-}


{-
A calling sequence table for the main functions is shown in the table below.

\begin{figure}[h]
\begin{center}
\begin{tabular}{|l|c|c|c|c|c|c|c|}d
\hline
{\bf A Call to} & socket & connect & bindSocket & listen & accept & read & write \\
\hline
{\bf Precedes} & & & & & & & \\
\hline 
socket &	&	  &	       &	&	 &	& \\
\hline
connect & +	&	  &	       &	&	 &	& \\
\hline
bindSocket & +	&	  &	       &	&	 &	& \\
\hline
listen &	&	  & +	       &	&	 &	& \\
\hline
accept &	&	  &	       &  +	&	 &	& \\
\hline
read   &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
write  &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
\end{tabular}
\caption{Sequence Table for Major functions of Socket}
\label{tab:api-seq}
\end{center}
\end{figure}
-}

-- ---------------------------------------------------------------------------
-- OS Dependent Definitions
    
unpackFamily	:: CInt -> Family
packFamily	:: Family -> CInt

packSocketType	:: SocketType -> CInt
unpackSocketType:: CInt -> SocketType

------ ------
			
packFamily f = case f of
	AF_UNSPEC -> 0
{-# LINE 1114 "Socket.hsc" #-}

{-# LINE 1115 "Socket.hsc" #-}
	AF_UNIX -> 1
{-# LINE 1116 "Socket.hsc" #-}

{-# LINE 1117 "Socket.hsc" #-}

{-# LINE 1118 "Socket.hsc" #-}
	AF_INET -> 2
{-# LINE 1119 "Socket.hsc" #-}

{-# LINE 1120 "Socket.hsc" #-}

{-# LINE 1121 "Socket.hsc" #-}
        AF_INET6 -> 10
{-# LINE 1122 "Socket.hsc" #-}

{-# LINE 1123 "Socket.hsc" #-}

{-# LINE 1126 "Socket.hsc" #-}

{-# LINE 1129 "Socket.hsc" #-}

{-# LINE 1132 "Socket.hsc" #-}

{-# LINE 1135 "Socket.hsc" #-}

{-# LINE 1138 "Socket.hsc" #-}

{-# LINE 1141 "Socket.hsc" #-}

{-# LINE 1144 "Socket.hsc" #-}

{-# LINE 1147 "Socket.hsc" #-}

{-# LINE 1148 "Socket.hsc" #-}
	AF_SNA -> 22
{-# LINE 1149 "Socket.hsc" #-}

{-# LINE 1150 "Socket.hsc" #-}

{-# LINE 1151 "Socket.hsc" #-}
	AF_DECnet -> 12
{-# LINE 1152 "Socket.hsc" #-}

{-# LINE 1153 "Socket.hsc" #-}

{-# LINE 1156 "Socket.hsc" #-}

{-# LINE 1159 "Socket.hsc" #-}

{-# LINE 1162 "Socket.hsc" #-}

{-# LINE 1163 "Socket.hsc" #-}
	AF_APPLETALK -> 5
{-# LINE 1164 "Socket.hsc" #-}

{-# LINE 1165 "Socket.hsc" #-}

{-# LINE 1166 "Socket.hsc" #-}
	AF_ROUTE -> 16
{-# LINE 1167 "Socket.hsc" #-}

{-# LINE 1168 "Socket.hsc" #-}

{-# LINE 1171 "Socket.hsc" #-}

{-# LINE 1174 "Socket.hsc" #-}

{-# LINE 1177 "Socket.hsc" #-}

{-# LINE 1180 "Socket.hsc" #-}

{-# LINE 1183 "Socket.hsc" #-}

{-# LINE 1186 "Socket.hsc" #-}

{-# LINE 1187 "Socket.hsc" #-}
	AF_X25 -> 9
{-# LINE 1188 "Socket.hsc" #-}

{-# LINE 1189 "Socket.hsc" #-}

{-# LINE 1190 "Socket.hsc" #-}
	AF_AX25 -> 3
{-# LINE 1191 "Socket.hsc" #-}

{-# LINE 1192 "Socket.hsc" #-}

{-# LINE 1195 "Socket.hsc" #-}

{-# LINE 1198 "Socket.hsc" #-}

{-# LINE 1199 "Socket.hsc" #-}
	AF_IPX -> 4
{-# LINE 1200 "Socket.hsc" #-}

{-# LINE 1201 "Socket.hsc" #-}

{-# LINE 1204 "Socket.hsc" #-}

{-# LINE 1207 "Socket.hsc" #-}

{-# LINE 1210 "Socket.hsc" #-}

{-# LINE 1213 "Socket.hsc" #-}

{-# LINE 1216 "Socket.hsc" #-}

{-# LINE 1219 "Socket.hsc" #-}

{-# LINE 1222 "Socket.hsc" #-}

{-# LINE 1225 "Socket.hsc" #-}

{-# LINE 1228 "Socket.hsc" #-}

{-# LINE 1231 "Socket.hsc" #-}

{-# LINE 1234 "Socket.hsc" #-}

{-# LINE 1237 "Socket.hsc" #-}

{-# LINE 1238 "Socket.hsc" #-}
        AF_ISDN -> 34
{-# LINE 1239 "Socket.hsc" #-}

{-# LINE 1240 "Socket.hsc" #-}

{-# LINE 1243 "Socket.hsc" #-}

{-# LINE 1246 "Socket.hsc" #-}

{-# LINE 1249 "Socket.hsc" #-}

{-# LINE 1252 "Socket.hsc" #-}

{-# LINE 1255 "Socket.hsc" #-}

{-# LINE 1258 "Socket.hsc" #-}

{-# LINE 1261 "Socket.hsc" #-}

{-# LINE 1264 "Socket.hsc" #-}

{-# LINE 1265 "Socket.hsc" #-}
	AF_NETROM -> 6
{-# LINE 1266 "Socket.hsc" #-}

{-# LINE 1267 "Socket.hsc" #-}

{-# LINE 1268 "Socket.hsc" #-}
	AF_BRIDGE -> 7
{-# LINE 1269 "Socket.hsc" #-}

{-# LINE 1270 "Socket.hsc" #-}

{-# LINE 1271 "Socket.hsc" #-}
	AF_ATMPVC -> 8
{-# LINE 1272 "Socket.hsc" #-}

{-# LINE 1273 "Socket.hsc" #-}

{-# LINE 1274 "Socket.hsc" #-}
	AF_ROSE -> 11
{-# LINE 1275 "Socket.hsc" #-}

{-# LINE 1276 "Socket.hsc" #-}

{-# LINE 1277 "Socket.hsc" #-}
	AF_NETBEUI -> 13
{-# LINE 1278 "Socket.hsc" #-}

{-# LINE 1279 "Socket.hsc" #-}

{-# LINE 1280 "Socket.hsc" #-}
	AF_SECURITY -> 14
{-# LINE 1281 "Socket.hsc" #-}

{-# LINE 1282 "Socket.hsc" #-}

{-# LINE 1283 "Socket.hsc" #-}
	AF_PACKET -> 17
{-# LINE 1284 "Socket.hsc" #-}

{-# LINE 1285 "Socket.hsc" #-}

{-# LINE 1286 "Socket.hsc" #-}
	AF_ASH -> 18
{-# LINE 1287 "Socket.hsc" #-}

{-# LINE 1288 "Socket.hsc" #-}

{-# LINE 1289 "Socket.hsc" #-}
	AF_ECONET -> 19
{-# LINE 1290 "Socket.hsc" #-}

{-# LINE 1291 "Socket.hsc" #-}

{-# LINE 1292 "Socket.hsc" #-}
	AF_ATMSVC -> 20
{-# LINE 1293 "Socket.hsc" #-}

{-# LINE 1294 "Socket.hsc" #-}

{-# LINE 1295 "Socket.hsc" #-}
	AF_IRDA -> 23
{-# LINE 1296 "Socket.hsc" #-}

{-# LINE 1297 "Socket.hsc" #-}

{-# LINE 1298 "Socket.hsc" #-}
	AF_PPPOX -> 24
{-# LINE 1299 "Socket.hsc" #-}

{-# LINE 1300 "Socket.hsc" #-}

{-# LINE 1301 "Socket.hsc" #-}
	AF_WANPIPE -> 25
{-# LINE 1302 "Socket.hsc" #-}

{-# LINE 1303 "Socket.hsc" #-}

{-# LINE 1304 "Socket.hsc" #-}
	AF_BLUETOOTH -> 31
{-# LINE 1305 "Socket.hsc" #-}

{-# LINE 1306 "Socket.hsc" #-}

--------- ----------

unpackFamily f = case f of
	(0) -> AF_UNSPEC
{-# LINE 1311 "Socket.hsc" #-}

{-# LINE 1312 "Socket.hsc" #-}
	(1) -> AF_UNIX
{-# LINE 1313 "Socket.hsc" #-}

{-# LINE 1314 "Socket.hsc" #-}

{-# LINE 1315 "Socket.hsc" #-}
	(2) -> AF_INET
{-# LINE 1316 "Socket.hsc" #-}

{-# LINE 1317 "Socket.hsc" #-}

{-# LINE 1318 "Socket.hsc" #-}
        (10) -> AF_INET6
{-# LINE 1319 "Socket.hsc" #-}

{-# LINE 1320 "Socket.hsc" #-}

{-# LINE 1323 "Socket.hsc" #-}

{-# LINE 1326 "Socket.hsc" #-}

{-# LINE 1329 "Socket.hsc" #-}

{-# LINE 1332 "Socket.hsc" #-}

{-# LINE 1335 "Socket.hsc" #-}

{-# LINE 1338 "Socket.hsc" #-}

{-# LINE 1341 "Socket.hsc" #-}

{-# LINE 1344 "Socket.hsc" #-}

{-# LINE 1345 "Socket.hsc" #-}
	(22) -> AF_SNA
{-# LINE 1346 "Socket.hsc" #-}

{-# LINE 1347 "Socket.hsc" #-}

{-# LINE 1348 "Socket.hsc" #-}
	(12) -> AF_DECnet
{-# LINE 1349 "Socket.hsc" #-}

{-# LINE 1350 "Socket.hsc" #-}

{-# LINE 1353 "Socket.hsc" #-}

{-# LINE 1356 "Socket.hsc" #-}

{-# LINE 1359 "Socket.hsc" #-}

{-# LINE 1360 "Socket.hsc" #-}
	(5) -> AF_APPLETALK
{-# LINE 1361 "Socket.hsc" #-}

{-# LINE 1362 "Socket.hsc" #-}

{-# LINE 1363 "Socket.hsc" #-}
	(16) -> AF_ROUTE
{-# LINE 1364 "Socket.hsc" #-}

{-# LINE 1365 "Socket.hsc" #-}

{-# LINE 1368 "Socket.hsc" #-}

{-# LINE 1371 "Socket.hsc" #-}

{-# LINE 1374 "Socket.hsc" #-}

{-# LINE 1377 "Socket.hsc" #-}

{-# LINE 1382 "Socket.hsc" #-}

{-# LINE 1385 "Socket.hsc" #-}

{-# LINE 1386 "Socket.hsc" #-}
	(9) -> AF_X25
{-# LINE 1387 "Socket.hsc" #-}

{-# LINE 1388 "Socket.hsc" #-}

{-# LINE 1389 "Socket.hsc" #-}
	(3) -> AF_AX25
{-# LINE 1390 "Socket.hsc" #-}

{-# LINE 1391 "Socket.hsc" #-}

{-# LINE 1394 "Socket.hsc" #-}

{-# LINE 1397 "Socket.hsc" #-}

{-# LINE 1398 "Socket.hsc" #-}
	(4) -> AF_IPX
{-# LINE 1399 "Socket.hsc" #-}

{-# LINE 1400 "Socket.hsc" #-}

{-# LINE 1403 "Socket.hsc" #-}

{-# LINE 1406 "Socket.hsc" #-}

{-# LINE 1409 "Socket.hsc" #-}

{-# LINE 1412 "Socket.hsc" #-}

{-# LINE 1415 "Socket.hsc" #-}

{-# LINE 1418 "Socket.hsc" #-}

{-# LINE 1421 "Socket.hsc" #-}

{-# LINE 1424 "Socket.hsc" #-}

{-# LINE 1427 "Socket.hsc" #-}

{-# LINE 1430 "Socket.hsc" #-}

{-# LINE 1433 "Socket.hsc" #-}

{-# LINE 1436 "Socket.hsc" #-}

{-# LINE 1437 "Socket.hsc" #-}
        (34) -> AF_ISDN
{-# LINE 1438 "Socket.hsc" #-}

{-# LINE 1439 "Socket.hsc" #-}

{-# LINE 1442 "Socket.hsc" #-}

{-# LINE 1445 "Socket.hsc" #-}

{-# LINE 1448 "Socket.hsc" #-}

{-# LINE 1451 "Socket.hsc" #-}

{-# LINE 1454 "Socket.hsc" #-}

{-# LINE 1457 "Socket.hsc" #-}

{-# LINE 1460 "Socket.hsc" #-}

{-# LINE 1463 "Socket.hsc" #-}

{-# LINE 1464 "Socket.hsc" #-}
	(6) -> AF_NETROM
{-# LINE 1465 "Socket.hsc" #-}

{-# LINE 1466 "Socket.hsc" #-}

{-# LINE 1467 "Socket.hsc" #-}
	(7) -> AF_BRIDGE
{-# LINE 1468 "Socket.hsc" #-}

{-# LINE 1469 "Socket.hsc" #-}

{-# LINE 1470 "Socket.hsc" #-}
	(8) -> AF_ATMPVC
{-# LINE 1471 "Socket.hsc" #-}

{-# LINE 1472 "Socket.hsc" #-}

{-# LINE 1473 "Socket.hsc" #-}
	(11) -> AF_ROSE
{-# LINE 1474 "Socket.hsc" #-}

{-# LINE 1475 "Socket.hsc" #-}

{-# LINE 1476 "Socket.hsc" #-}
	(13) -> AF_NETBEUI
{-# LINE 1477 "Socket.hsc" #-}

{-# LINE 1478 "Socket.hsc" #-}

{-# LINE 1479 "Socket.hsc" #-}
	(14) -> AF_SECURITY
{-# LINE 1480 "Socket.hsc" #-}

{-# LINE 1481 "Socket.hsc" #-}

{-# LINE 1482 "Socket.hsc" #-}
	(17) -> AF_PACKET
{-# LINE 1483 "Socket.hsc" #-}

{-# LINE 1484 "Socket.hsc" #-}

{-# LINE 1485 "Socket.hsc" #-}
	(18) -> AF_ASH
{-# LINE 1486 "Socket.hsc" #-}

{-# LINE 1487 "Socket.hsc" #-}

{-# LINE 1488 "Socket.hsc" #-}
	(19) -> AF_ECONET
{-# LINE 1489 "Socket.hsc" #-}

{-# LINE 1490 "Socket.hsc" #-}

{-# LINE 1491 "Socket.hsc" #-}
	(20) -> AF_ATMSVC
{-# LINE 1492 "Socket.hsc" #-}

{-# LINE 1493 "Socket.hsc" #-}

{-# LINE 1494 "Socket.hsc" #-}
	(23) -> AF_IRDA
{-# LINE 1495 "Socket.hsc" #-}

{-# LINE 1496 "Socket.hsc" #-}

{-# LINE 1497 "Socket.hsc" #-}
	(24) -> AF_PPPOX
{-# LINE 1498 "Socket.hsc" #-}

{-# LINE 1499 "Socket.hsc" #-}

{-# LINE 1500 "Socket.hsc" #-}
	(25) -> AF_WANPIPE
{-# LINE 1501 "Socket.hsc" #-}

{-# LINE 1502 "Socket.hsc" #-}

{-# LINE 1503 "Socket.hsc" #-}
	(31) -> AF_BLUETOOTH
{-# LINE 1504 "Socket.hsc" #-}

{-# LINE 1505 "Socket.hsc" #-}
	unknown -> error ("Network.Socket.unpackFamily: unknown address " ++
                          "family " ++ show unknown)

-- Socket Types.

-- | Socket Types.
--
-- This data type might have different constructors depending on what is
-- supported by the operating system.
data SocketType
	= NoSocketType

{-# LINE 1517 "Socket.hsc" #-}
	| Stream 

{-# LINE 1519 "Socket.hsc" #-}

{-# LINE 1520 "Socket.hsc" #-}
	| Datagram

{-# LINE 1522 "Socket.hsc" #-}

{-# LINE 1523 "Socket.hsc" #-}
	| Raw 

{-# LINE 1525 "Socket.hsc" #-}

{-# LINE 1526 "Socket.hsc" #-}
	| RDM 

{-# LINE 1528 "Socket.hsc" #-}

{-# LINE 1529 "Socket.hsc" #-}
	| SeqPacket

{-# LINE 1531 "Socket.hsc" #-}
	deriving (Eq, Ord, Read, Show)
	
INSTANCE_TYPEABLE0(SocketType,socketTypeTc,"SocketType")

packSocketType stype = case stype of
	NoSocketType -> 0

{-# LINE 1538 "Socket.hsc" #-}
	Stream -> 1
{-# LINE 1539 "Socket.hsc" #-}

{-# LINE 1540 "Socket.hsc" #-}

{-# LINE 1541 "Socket.hsc" #-}
	Datagram -> 2
{-# LINE 1542 "Socket.hsc" #-}

{-# LINE 1543 "Socket.hsc" #-}

{-# LINE 1544 "Socket.hsc" #-}
	Raw -> 3
{-# LINE 1545 "Socket.hsc" #-}

{-# LINE 1546 "Socket.hsc" #-}

{-# LINE 1547 "Socket.hsc" #-}
	RDM -> 4
{-# LINE 1548 "Socket.hsc" #-}

{-# LINE 1549 "Socket.hsc" #-}

{-# LINE 1550 "Socket.hsc" #-}
	SeqPacket -> 5
{-# LINE 1551 "Socket.hsc" #-}

{-# LINE 1552 "Socket.hsc" #-}

unpackSocketType t = case t of
	0 -> NoSocketType

{-# LINE 1556 "Socket.hsc" #-}
	(1) -> Stream
{-# LINE 1557 "Socket.hsc" #-}

{-# LINE 1558 "Socket.hsc" #-}

{-# LINE 1559 "Socket.hsc" #-}
	(2) -> Datagram
{-# LINE 1560 "Socket.hsc" #-}

{-# LINE 1561 "Socket.hsc" #-}

{-# LINE 1562 "Socket.hsc" #-}
	(3) -> Raw
{-# LINE 1563 "Socket.hsc" #-}

{-# LINE 1564 "Socket.hsc" #-}

{-# LINE 1565 "Socket.hsc" #-}
	(4) -> RDM
{-# LINE 1566 "Socket.hsc" #-}

{-# LINE 1567 "Socket.hsc" #-}

{-# LINE 1568 "Socket.hsc" #-}
	(5) -> SeqPacket
{-# LINE 1569 "Socket.hsc" #-}

{-# LINE 1570 "Socket.hsc" #-}

-- ---------------------------------------------------------------------------
-- Utility Functions

aNY_PORT :: PortNumber 
aNY_PORT = 0

-- | The IPv4 wild card address.

iNADDR_ANY :: HostAddress
iNADDR_ANY = htonl (0)
{-# LINE 1581 "Socket.hsc" #-}


{-# LINE 1583 "Socket.hsc" #-}
-- | The IPv6 wild card address.

iN6ADDR_ANY :: HostAddress6
iN6ADDR_ANY = (0, 0, 0, 0)

{-# LINE 1588 "Socket.hsc" #-}

sOMAXCONN :: Int
sOMAXCONN = 128
{-# LINE 1591 "Socket.hsc" #-}

sOL_SOCKET :: Int
sOL_SOCKET = 1
{-# LINE 1594 "Socket.hsc" #-}


{-# LINE 1596 "Socket.hsc" #-}
sCM_RIGHTS :: Int
sCM_RIGHTS = 1
{-# LINE 1598 "Socket.hsc" #-}

{-# LINE 1599 "Socket.hsc" #-}

maxListenQueue :: Int
maxListenQueue = sOMAXCONN

-- -----------------------------------------------------------------------------

data ShutdownCmd 
 = ShutdownReceive
 | ShutdownSend
 | ShutdownBoth

INSTANCE_TYPEABLE0(ShutdownCmd,shutdownCmdTc,"ShutdownCmd")

sdownCmdToInt :: ShutdownCmd -> CInt
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (MkSocket s _ _ _ _) stype = do
  throwSocketErrorIfMinus1Retry "shutdown" (c_shutdown s (sdownCmdToInt stype))
  return ()

-- -----------------------------------------------------------------------------

-- | Closes a socket
sClose	 :: Socket -> IO ()
sClose (MkSocket s _ _ _ socketStatus) = do 
 modifyMVar_ socketStatus $ \ status ->
   case status of
     ConvertedToHandle ->
	 ioError (userError ("sClose: converted to a Handle, use hClose instead"))
     Closed ->
	 return status
     _ -> c_close s >> return Closed

-- -----------------------------------------------------------------------------

sIsConnected :: Socket -> IO Bool
sIsConnected (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Connected)	

-- -----------------------------------------------------------------------------
-- Socket Predicates

sIsBound :: Socket -> IO Bool
sIsBound (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Bound)	

sIsListening :: Socket -> IO Bool
sIsListening (MkSocket _ _ _  _ status) = do
    value <- readMVar status
    return (value == Listening)	

sIsReadable  :: Socket -> IO Bool
sIsReadable (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Listening || value == Connected)

sIsWritable  :: Socket -> IO Bool
sIsWritable = sIsReadable -- sort of.

sIsAcceptable :: Socket -> IO Bool

{-# LINE 1665 "Socket.hsc" #-}
sIsAcceptable (MkSocket _ AF_UNIX Stream _ status) = do
    value <- readMVar status
    return (value == Connected || value == Bound || value == Listening)
sIsAcceptable (MkSocket _ AF_UNIX _ _ _) = return False

{-# LINE 1670 "Socket.hsc" #-}
sIsAcceptable (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Connected || value == Listening)
    
-- -----------------------------------------------------------------------------
-- Internet address manipulation routines:

inet_addr :: String -> IO HostAddress
inet_addr ipstr = do
   withCString ipstr $ \str -> do
   had <- c_inet_addr str
   if had == -1
    then ioError (userError ("inet_addr: Malformed address: " ++ ipstr))
    else return had  -- network byte order

inet_ntoa :: HostAddress -> IO String
inet_ntoa haddr = do
  pstr <- c_inet_ntoa haddr
  peekCString pstr

-- | turns a Socket into an 'Handle'. By default, the new handle is
-- unbuffered. Use 'System.IO.hSetBuffering' to change the buffering.
--
-- Note that since a 'Handle' is automatically closed by a finalizer
-- when it is no longer referenced, you should avoid doing any more
-- operations on the 'Socket' after calling 'socketToHandle'.  To
-- close the 'Socket' after 'socketToHandle', call 'System.IO.hClose'
-- on the 'Handle'.


{-# LINE 1700 "Socket.hsc" #-}
socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle s@(MkSocket fd _ _ _ socketStatus) mode = do
 modifyMVar socketStatus $ \ status ->
    if status == ConvertedToHandle
	then ioError (userError ("socketToHandle: already a Handle"))
	else do

{-# LINE 1707 "Socket.hsc" #-}
    h <- fdToHandle' (fromIntegral fd) (Just System.Posix.Internals.Stream) True (show s) mode True{-bin-}

{-# LINE 1713 "Socket.hsc" #-}
    return (ConvertedToHandle, h)

{-# LINE 1718 "Socket.hsc" #-}

-- | Pack a list of values into a bitmask.  The possible mappings from
-- value to bit-to-set are given as the first argument.  We assume
-- that each value can cause exactly one bit to be set; unpackBits will
-- break if this property is not true.

packBits :: (Eq a, Bits b) => [(a, b)] -> [a] -> b

packBits mapping xs = foldl' pack 0 mapping
    where pack acc (k, v) | k `elem` xs = acc .|. v
                          | otherwise   = acc

-- | Unpack a bitmask into a list of values.

unpackBits :: Bits b => [(a, b)] -> b -> [a]

-- Be permissive and ignore unknown bit values. At least on OS X,
-- getaddrinfo returns an ai_flags field with bits set that have no
-- entry in <netdb.h>.
unpackBits [] _    = []
unpackBits ((k,v):xs) r
    | r .&. v /= 0 = k : unpackBits xs (r .&. complement v)
    | otherwise    = unpackBits xs r

-----------------------------------------------------------------------------
-- Address and service lookups


{-# LINE 1746 "Socket.hsc" #-}

-- | Flags that control the querying behaviour of 'getAddrInfo'.
data AddrInfoFlag
    = AI_ADDRCONFIG
    | AI_ALL
    | AI_CANONNAME
    | AI_NUMERICHOST
    | AI_NUMERICSERV
    | AI_PASSIVE
    | AI_V4MAPPED
    deriving (Eq, Read, Show)

INSTANCE_TYPEABLE0(AddrInfoFlag,addrInfoFlagTc,"AddrInfoFlag")

aiFlagMapping :: [(AddrInfoFlag, CInt)]

aiFlagMapping =
    [

{-# LINE 1765 "Socket.hsc" #-}
     (AI_ADDRCONFIG, 32),
{-# LINE 1766 "Socket.hsc" #-}

{-# LINE 1769 "Socket.hsc" #-}

{-# LINE 1770 "Socket.hsc" #-}
     (AI_ALL, 16),
{-# LINE 1771 "Socket.hsc" #-}

{-# LINE 1774 "Socket.hsc" #-}
     (AI_CANONNAME, 2),
{-# LINE 1775 "Socket.hsc" #-}
     (AI_NUMERICHOST, 4),
{-# LINE 1776 "Socket.hsc" #-}

{-# LINE 1779 "Socket.hsc" #-}
     (AI_NUMERICSERV, 0),

{-# LINE 1781 "Socket.hsc" #-}
     (AI_PASSIVE, 1),
{-# LINE 1782 "Socket.hsc" #-}

{-# LINE 1783 "Socket.hsc" #-}
     (AI_V4MAPPED, 8)
{-# LINE 1784 "Socket.hsc" #-}

{-# LINE 1787 "Socket.hsc" #-}
    ]

-- | Indicate whether the given 'AddrInfoFlag' will have any effect on
-- this system.
addrInfoFlagImplemented :: AddrInfoFlag -> Bool
addrInfoFlagImplemented f = packBits aiFlagMapping [f] /= 0

data AddrInfo =
    AddrInfo {
        addrFlags :: [AddrInfoFlag],
        addrFamily :: Family,
        addrSocketType :: SocketType,
        addrProtocol :: ProtocolNumber,
        addrAddress :: SockAddr,
        addrCanonName :: Maybe String
        }
    deriving (Eq, Show)

INSTANCE_TYPEABLE0(AddrInfo,addrInfoTc,"AddrInfo")

instance Storable AddrInfo where
    sizeOf    _ = 32
{-# LINE 1809 "Socket.hsc" #-}
    alignment _ = alignment (undefined :: CInt)

    peek p = do
	ai_flags <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 1813 "Socket.hsc" #-}
	ai_family <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 1814 "Socket.hsc" #-}
	ai_socktype <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 1815 "Socket.hsc" #-}
	ai_protocol <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p
{-# LINE 1816 "Socket.hsc" #-}
        ai_addr <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) p >>= peekSockAddr
{-# LINE 1817 "Socket.hsc" #-}
	ai_canonname_ptr <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 1818 "Socket.hsc" #-}

        ai_canonname <- if ai_canonname_ptr == nullPtr
                        then return Nothing
                        else liftM Just $ peekCString ai_canonname_ptr
                             
        return (AddrInfo
                {
                 addrFlags = unpackBits aiFlagMapping ai_flags,
                 addrFamily = unpackFamily ai_family,
                 addrSocketType = unpackSocketType ai_socktype,
                 addrProtocol = ai_protocol,
                 addrAddress = ai_addr,
                 addrCanonName = ai_canonname
                })

    poke p (AddrInfo flags family socketType protocol _ _) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (packBits aiFlagMapping flags)
{-# LINE 1835 "Socket.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p (packFamily family)
{-# LINE 1836 "Socket.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p (packSocketType socketType)
{-# LINE 1837 "Socket.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p protocol
{-# LINE 1838 "Socket.hsc" #-}

        -- stuff below is probably not needed, but let's zero it for safety

        ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p (0::CSize)
{-# LINE 1842 "Socket.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) p nullPtr
{-# LINE 1843 "Socket.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) p nullPtr
{-# LINE 1844 "Socket.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 28)) p nullPtr
{-# LINE 1845 "Socket.hsc" #-}

data NameInfoFlag
    = NI_DGRAM
    | NI_NAMEREQD
    | NI_NOFQDN
    | NI_NUMERICHOST
    | NI_NUMERICSERV
    deriving (Eq, Read, Show)

INSTANCE_TYPEABLE0(NameInfoFlag,nameInfoFlagTc,"NameInfoFlag")

niFlagMapping :: [(NameInfoFlag, CInt)]

niFlagMapping = [(NI_DGRAM, 16),
{-# LINE 1859 "Socket.hsc" #-}
                 (NI_NAMEREQD, 8),
{-# LINE 1860 "Socket.hsc" #-}
                 (NI_NOFQDN, 4),
{-# LINE 1861 "Socket.hsc" #-}
                 (NI_NUMERICHOST, 1),
{-# LINE 1862 "Socket.hsc" #-}
                 (NI_NUMERICSERV, 2)]
{-# LINE 1863 "Socket.hsc" #-}

-- | Default hints for address lookup with 'getAddrInfo'.  The values
-- of the 'addrAddress' and 'addrCanonName' fields are 'undefined',
-- and are never inspected by 'getAddrInfo'.

defaultHints :: AddrInfo

defaultHints = AddrInfo {
                         addrFlags = [],
                         addrFamily = AF_UNSPEC,
                         addrSocketType = NoSocketType,
                         addrProtocol = defaultProtocol,
                         addrAddress = undefined,
                         addrCanonName = undefined
                        }

-- | Resolve a host or service name to one or more addresses.
-- The 'AddrInfo' values that this function returns contain 'SockAddr'
-- values that you can pass directly to 'connect' or
-- 'bindSocket'.
--
-- This function is protocol independent.  It can return both IPv4 and
-- IPv6 address information.
--
-- The 'AddrInfo' argument specifies the preferred query behaviour,
-- socket options, or protocol.  You can override these conveniently
-- using Haskell's record update syntax on 'defaultHints', for example
-- as follows:
--
-- @
--   myHints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME] }
-- @
--
-- Values for 'addrFlags' control query behaviour.  The supported
-- flags are as follows:
--
--   [@AI_PASSIVE@] If no 'HostName' value is provided, the network
--     address in each 'SockAddr'
--     will be left as a "wild card", i.e. as either 'iNADDR_ANY'
--     or 'iN6ADDR_ANY'.  This is useful for server applications that
--     will accept connections from any client.
--
--   [@AI_CANONNAME@] The 'addrCanonName' field of the first returned
--     'AddrInfo' will contain the "canonical name" of the host.
--
--   [@AI_NUMERICHOST@] The 'HostName' argument /must/ be a numeric
--     address in string form, and network name lookups will not be
--     attempted.
-- 
-- /Note/: Although the following flags are required by RFC 3493, they
-- may not have an effect on all platforms, because the underlying
-- network stack may not support them.  To see whether a flag from the
-- list below will have any effect, call 'addrInfoFlagImplemented'.
--
--   [@AI_NUMERICSERV@] The 'ServiceName' argument /must/ be a port
--     number in string form, and service name lookups will not be
--     attempted.
--
--   [@AI_ADDRCONFIG@] The list of returned 'AddrInfo' values will
--     only contain IPv4 addresses if the local system has at least
--     one IPv4 interface configured, and likewise for IPv6.
--
--   [@AI_V4MAPPED@] If an IPv6 lookup is performed, and no IPv6
--     addresses are found, IPv6-mapped IPv4 addresses will be
--     returned.
--
--   [@AI_ALL@] If 'AI_ALL' is specified, return all matching IPv6 and
--     IPv4 addresses.  Otherwise, this flag has no effect.
--     
-- You must provide a 'Just' value for at least one of the 'HostName'
-- or 'ServiceName' arguments.  'HostName' can be either a numeric
-- network address (dotted quad for IPv4, colon-separated hex for
-- IPv6) or a hostname.  In the latter case, its addresses will be
-- looked up unless 'AI_NUMERICHOST' is specified as a hint.  If you
-- do not provide a 'HostName' value /and/ do not set 'AI_PASSIVE' as
-- a hint, network addresses in the result will contain the address of
-- the loopback interface.
--
-- If the query fails, this function throws an IO exception instead of
-- returning an empty list.  Otherwise, it returns a non-empty list
-- of 'AddrInfo' values.
--
-- There are several reasons why a query might result in several
-- values.  For example, the queried-for host could be multihomed, or
-- the service might be available via several protocols.
--
-- Note: the order of arguments is slightly different to that defined
-- for @getaddrinfo@ in RFC 2553.  The 'AddrInfo' parameter comes first
-- to make partial application easier.
--
-- Example:
-- @
--   let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME] }
--   addrs <- getAddrInfo (Just hints) (Just "www.haskell.org") (Just "http")
--   let addr = head addrs
--   sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--   connect sock (addrAddress addr)
-- @

getAddrInfo :: Maybe AddrInfo -- ^ preferred socket type or protocol
            -> Maybe HostName -- ^ host name to look up
            -> Maybe ServiceName -- ^ service name to look up
            -> IO [AddrInfo] -- ^ resolved addresses, with "best" first

getAddrInfo hints node service =
  maybeWith withCString node $ \c_node ->
    maybeWith withCString service $ \c_service ->
      maybeWith with hints $ \c_hints ->
        alloca $ \ptr_ptr_addrs -> do
          ret <- c_getaddrinfo c_node c_service c_hints ptr_ptr_addrs
          case ret of
            0 -> do ptr_addrs <- peek ptr_ptr_addrs
                    ais <- followAddrInfo ptr_addrs
                    c_freeaddrinfo ptr_addrs
                    return ais
            _ -> do err <- gai_strerror ret
                    ioError (ioeSetErrorString
                             (mkIOError NoSuchThing "getAddrInfo" Nothing
                              Nothing) err)

followAddrInfo :: Ptr AddrInfo -> IO [AddrInfo]

followAddrInfo ptr_ai | ptr_ai == nullPtr = return []
                      | otherwise = do
    a <- peek ptr_ai
    as <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr_ai >>= followAddrInfo
{-# LINE 1989 "Socket.hsc" #-}
    return (a:as)

foreign import ccall safe "hsnet_getaddrinfo"
    c_getaddrinfo :: CString -> CString -> Ptr AddrInfo -> Ptr (Ptr AddrInfo)
                  -> IO CInt

foreign import ccall safe "hsnet_freeaddrinfo"
    c_freeaddrinfo :: Ptr AddrInfo -> IO ()

gai_strerror :: CInt -> IO String


{-# LINE 2001 "Socket.hsc" #-}
gai_strerror n = c_gai_strerror n >>= peekCString

foreign import ccall safe "gai_strerror"
    c_gai_strerror :: CInt -> IO CString

{-# LINE 2008 "Socket.hsc" #-}

withCStringIf :: Bool -> Int -> (CSize -> CString -> IO a) -> IO a
withCStringIf False _ f = f 0 nullPtr
withCStringIf True n f = allocaBytes n (f (fromIntegral n))
                    
-- | Resolve an address to a host or service name.
-- This function is protocol independent.
--
-- The list of 'NameInfoFlag' values controls query behaviour.  The
-- supported flags are as follows:
--
--   [@NI_NOFQDN@] If a host is local, return only the
--     hostname part of the FQDN.
--
--   [@NI_NUMERICHOST@] The name of the host is not
--     looked up.  Instead, a numeric representation of the host's
--     address is returned.  For an IPv4 address, this will be a
--     dotted-quad string.  For IPv6, it will be colon-separated
--     hexadecimal.
--
--   [@NI_NUMERICSERV@] The name of the service is not
--     looked up.  Instead, a numeric representation of the
--     service is returned.
--
--   [@NI_NAMEREQD@] If the hostname cannot be looked up, an IO error
--     is thrown.
--
--   [@NI_DGRAM@] Resolve a datagram-based service name.  This is
--     required only for the few protocols that have different port
--     numbers for their datagram-based versions than for their
--     stream-based versions.
--
-- Hostname and service name lookups can be expensive.  You can
-- specify which lookups to perform via the two 'Bool' arguments.  If
-- one of these is 'False', the corresponding value in the returned
-- tuple will be 'Nothing', and no lookup will be performed.
--
-- If a host or service's name cannot be looked up, then the numeric
-- form of the address or service will be returned.
--
-- If the query fails, this function throws an IO exception.
--
-- Example:
-- @
--   (hostName, _) <- getNameInfo [] True False myAddress
-- @

getNameInfo :: [NameInfoFlag] -- ^ flags to control lookup behaviour
            -> Bool -- ^ whether to look up a hostname
            -> Bool -- ^ whether to look up a service name
            -> SockAddr -- ^ the address to look up
            -> IO (Maybe HostName, Maybe ServiceName)

getNameInfo flags doHost doService addr =
  withCStringIf doHost (1025) $ \c_hostlen c_host ->
{-# LINE 2063 "Socket.hsc" #-}
    withCStringIf doService (32) $ \c_servlen c_serv -> do
{-# LINE 2064 "Socket.hsc" #-}
      withSockAddr addr $ \ptr_addr sz -> do
        ret <- c_getnameinfo ptr_addr (fromIntegral sz) c_host c_hostlen
                             c_serv c_servlen (packBits niFlagMapping flags)
        case ret of
          0 -> do
            let peekIf doIf c_val = if doIf
                                     then liftM Just $ peekCString c_val
                                     else return Nothing
            host <- peekIf doHost c_host
            serv <- peekIf doService c_serv
            return (host, serv)
          _ -> do err <- gai_strerror ret
                  ioError (ioeSetErrorString
                           (mkIOError NoSuchThing "getNameInfo" Nothing 
                            Nothing) err)

foreign import ccall safe "hsnet_getnameinfo"
    c_getnameinfo :: Ptr SockAddr -> CInt{-CSockLen???-} -> CString -> CSize -> CString
                  -> CSize -> CInt -> IO CInt

{-# LINE 2084 "Socket.hsc" #-}

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError

{-# LINE 2088 "Socket.hsc" #-}
				    InvalidArgument

{-# LINE 2092 "Socket.hsc" #-}
                                    loc Nothing Nothing) "non-positive length"

mkEOFError :: String -> IOError
mkEOFError loc = ioeSetErrorString (mkIOError EOF loc Nothing Nothing) "end of file"

-- ---------------------------------------------------------------------------
-- foreign imports from the C library

foreign import ccall unsafe "my_inet_ntoa"
  c_inet_ntoa :: HostAddress -> IO (Ptr CChar)

foreign import CALLCONV unsafe "inet_addr"
  c_inet_addr :: Ptr CChar -> IO HostAddress

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt 


{-# LINE 2110 "Socket.hsc" #-}
foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt

{-# LINE 2116 "Socket.hsc" #-}

foreign import CALLCONV unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import CALLCONV unsafe "bind"
  c_bind :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "connect"
  c_connect :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "accept"
  c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV safe "accept"
  c_accept_safe :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "listen"
  c_listen :: CInt -> CInt -> IO CInt


{-# LINE 2131 "Socket.hsc" #-}
foreign import ccall "rtsSupportsBoundThreads" threaded :: Bool

{-# LINE 2133 "Socket.hsc" #-}

foreign import CALLCONV unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "sendto"
  c_sendto :: CInt -> Ptr a -> CSize -> CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import CALLCONV unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "recvfrom"
  c_recvfrom :: CInt -> Ptr a -> CSize -> CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getpeername"
  c_getpeername :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getsockname"
  c_getsockname :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
