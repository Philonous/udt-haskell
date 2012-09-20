#include <sys/socket.h>
#include "helpers.h"

int af_inet()  {return AF_INET;  }
int af_inet6() {return AF_INET6; }

int sock_stream() {return SOCK_STREAM; }
int sock_dgram() {return SOCK_DGRAM; }
