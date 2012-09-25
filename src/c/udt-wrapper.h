#ifdef __cplusplus
  #include <udt/udt.h>
  using namespace UDT;
  extern "C"  {
#else
  extern const int ERROR;
  enum UDTOpt
  {
     UDT_MSS,             // the Maximum Transfer Unit
     UDT_SNDSYN,          // if sending is blocking
     UDT_RCVSYN,          // if receiving is blocking
     UDT_CC,              // custom congestion control algorithm
     UDT_FC,		// Flight flag size (window size)
     UDT_SNDBUF,          // maximum buffer in sending queue
     UDT_RCVBUF,          // UDT receiving buffer size
     UDT_LINGER,          // waiting for unsent data when closing
     UDP_SNDBUF,          // UDP sending buffer size
     UDP_RCVBUF,          // UDP receiving buffer size
     UDT_MAXMSG,          // maximum datagram message size
     UDT_MSGTTL,          // time-to-live of a datagram message
     UDT_RENDEZVOUS,      // rendezvous connection mode
     UDT_SNDTIMEO,        // send() timeout
     UDT_RCVTIMEO,        // recv() timeout
     UDT_REUSEADDR,	// reuse an existing port or create a new one
     UDT_MAXBW,		// maximum bandwidth (bytes per second) that the connection can use
     UDT_STATE,		// current socket state, see UDTSTATUS, read only
     UDT_EVENT,		// current avalable events associated with the socket
     UDT_SNDDATA,		// size of data in the sending buffer
     UDT_RCVDATA		// size of data available for recv
  };

  typedef enum UDTOpt SOCKOPT;

  typedef struct ERRORINFO ERRORINFO;
  typedef int UDTSOCKET;
  #ifdef WIN32
     #ifndef __MINGW__
        typedef SOCKET SYSSOCKET;
     #else
        typedef int SYSSOCKET;
     #endif
  #else
     typedef int SYSSOCKET;
  #endif
  typedef SYSSOCKET UDPSOCKET;
#endif //__cplusplus

  typedef int BOOL;


  UDTSOCKET udt_socket( int af
                      , int type
                      , int protocol );

  int udt_listen (UDTSOCKET u, int backlog);

  UDTSOCKET udt_accept( UDTSOCKET u
                      , struct sockaddr* addr
                      , int* addrlen );

  int udt_bind( UDTSOCKET u
              , struct sockaddr* name
              , int namelen );

  int udt_bind_socket( UDTSOCKET u
                     , UDPSOCKET udpsock);

  int udt_cleanup();

  int udt_close(UDTSOCKET u);

  int udt_connect( UDTSOCKET u
                 , const struct sockaddr* name
                 , int namelen );

  ERRORINFO* udt_getlasterror();

  int udt_errorinfo_get_error_code(ERRORINFO* info);

  const char* udt_errorinfo_get_error_message(ERRORINFO* info);

  void udt_errorinfo_clear(ERRORINFO* info);

  int udt_getpeername( UDTSOCKET u
                     , struct sockaddr* name
                     , int* namelen);

  int udt_send( UDTSOCKET u
              , const char* buf
              , int len
              , int flags);

  int udt_recv( UDTSOCKET u
              , char* buf
              , int len
              , int flags);

  int udt_sendmsg( UDTSOCKET u
                 , const char* buf
                 , int len, int ttl
                 , BOOL inorder);

  int udt_recvmsg( UDTSOCKET u
                 , char* buf
                 , int len);

  int udt_startup();

  int udt_getsockopt( UDTSOCKET u
                    , int level
                    , SOCKOPT optname
                    , void* optval
                    , int* optlen);

  int udt_setsockopt( UDTSOCKET u
                    , int level
                    , SOCKOPT optname
                    , const void* optval
                    , int optlen);

  UDTSOCKET udt_invalid_sock();
  int udt_error();

#ifdef __cplusplus
  } // extern
#endif
