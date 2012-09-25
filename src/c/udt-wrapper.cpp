#include <udt/udt.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "udt-wrapper.h"
#include <cstdio>

using namespace UDT;

extern "C"  {

  UDTSOCKET udt_socket( int af
                      , int type
                      , int protocol ){
    return UDT::socket(af, type, protocol);
  }

  int udt_listen (UDTSOCKET u, int backlog) {
    return UDT::listen(u,backlog);
  }

  UDTSOCKET udt_accept( UDTSOCKET u
                      , struct sockaddr* addr
                      , int* addrlen ){
    return UDT::accept(u, addr, addrlen);
  }

  int udt_bind( UDTSOCKET u
              , struct sockaddr* name
              , int namelen ){ // TODO: check this!
    return UDT::bind(u, name, namelen);
  }

  int udt_bind_socket( UDTSOCKET u
                     , UDPSOCKET udpsock){
    return UDT::bind(u, udpsock);
  }

  int udt_cleanup(void){
    return UDT::cleanup();
  }

  int udt_close(UDTSOCKET u){
    return UDT::close(u);
  }

  int udt_connect( UDTSOCKET u
                 , const struct sockaddr* name
                 , int namelen ){
    return UDT::connect(u,name, namelen);
  }

  ERRORINFO* udt_getlasterror(){
    return & UDT::getlasterror();
  }

  int udt_errorinfo_get_error_code(ERRORINFO* info){
    return info->getErrorCode();
  }

  const char* udt_errorinfo_get_error_message(ERRORINFO* info){
    return info->getErrorMessage();
  }

  void udt_errorinfo_clear(ERRORINFO* info){
    info->clear();
  }

  int udt_getpeername( UDTSOCKET u
                     , struct sockaddr* name
                     , int* namelen){
    return UDT::getpeername(u,name, namelen);
  }

  int udt_getsockopt( UDTSOCKET u
                    , int level
                    , SOCKOPT optname
                    , void* optval
                    , int* optlen){
    return UDT::getsockopt(u, level, optname, optval, optlen) ;
  }

  int udt_send( UDTSOCKET u
              , const char* buf
              , int len
              , int flags){
    return UDT::send(u, buf, len, flags);
  }
  int udt_recv( UDTSOCKET u
              , char* buf
              , int len
              , int flags){
    return UDT::recv(u, buf, len, flags);
  }
  int udt_sendmsg( UDTSOCKET u
                 , const char* buf
                 , int len, int ttl = -1
                 , int inorder = 0){
    return UDT::sendmsg(u, buf, len, ttl, (bool)inorder);
  }
  int udt_recvmsg( UDTSOCKET u
                 , char* buf
                 , int len){
    return UDT::recvmsg(u, buf, len);
  }

  int udt_startup() {
    int errval = UDT::ERROR;
    printf("er %i \n", errval);
    printf("so %i \n", UDT::INVALID_SOCK);
    return UDT::startup();;
  }

  // error values
  UDTSOCKET udt_invalid_sock(){return UDT::INVALID_SOCK; }
  int udt_error(){return UDT::ERROR;}

  int udt_setsockopt( UDTSOCKET u
                    , int level
                    , SOCKOPT optname
                    , const void* optval
                    , int optlen){
    return UDT::setsockopt(u, level, optname, optval, optlen);
  }

 /* int64_t udt_sendfile(UDTSOCKET u, std::fstream& ifs, int64_t& offset, int64_t size, int block = 364000){} */
 /* int64_t udt_recvfile(UDTSOCKET u, std::fstream& ofs, int64_t& offset, int64_t size, int block = 7280000); */
 /*  int udt_epoll_create() { */
 /*    return epoll_create(); */
 /*  } */

 /*  int udt_epoll_add_usock(const int eid, */
 /*                          const UDTSOCKET usock, */
 /*                          const int* events = NULL ){ */
 /*    return epoll_add_usock(eid, usock, events); */
 /*  } */

 /*  int udt_epoll_add_ssock( const int eid */
 /*                         , const UDTSOCKET ssock */
 /*                         , const int* events = NULL ){ */
 /*    return epoll_add_ssock(eid, ssock, events); */
 /*  } */

 /*  int udt_epoll_remove_usock(const int eid */
 /*                            , const UDTSOCKET usock ){ */
 /*    return epoll_remove_usock(eid, usock); */
 /*  } */

 /*  int udt_epoll_remove_ssock( const int eid */
 /*                        , const UDTSOCKET ssock ){ */
 /*    return epoll_remove_ssock(eid, ssock); */
 /*  } */

 /* /\* int epoll_wait( const int eid *\/ */
 /* /\*               , std::set<UDTSOCKET>* readfds, std::set<UDTSOCKET>* writefds, int64_t msTimeOut, std::set<SYSSOCKET>* lrfds = NULL, std::set<SYSSOCKET>* wrfds = NULL); *\/ */

 /* int udt_epoll_release(const int eid){ */
 /*   return epoll_release(eid) */
 /* } */
} // extern
