#include <stdio.h>
#include <unistd.h>
#include <string.h> /* for strncpy */
#include <ifaddrs.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>
#include <arpa/inet.h>

/*
Return a string with the IP address (in the usual 'dot notation'
format) of the interface whose name is given as a parameter.
It works on Linux, I do not know about other Systems.

To test this function, compile this file with TEST defined
(e.g., with gcc add -DTEST to the command line) and run the
resulting executable.  You should see the interfaces present
on  your computer and their IP addresses (including "lo" with
address 127.0.0.1).

Status: <TESTED>
*/

char *
interface_to_ip(char *interface_name)
{
 int fd;
 struct ifreq ifr;

 fd = socket(AF_INET, SOCK_DGRAM, 0);

 /* I want to get an IPv4 IP address */
 ifr.ifr_addr.sa_family = AF_INET;

 /* I want IP address attached to interface_name */
 strncpy(ifr.ifr_name, interface_name, IFNAMSIZ-1);

 ioctl(fd, SIOCGIFADDR, &ifr);

 close(fd);

 return inet_ntoa(((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr);
}

char *
interface_name(int index)
{
  struct ifaddrs *ifap, *cursor;

  getifaddrs(&ifap);

  for(cursor=ifap; cursor != NULL && index > 0; cursor = cursor->ifa_next)
    { --index; }

  if (cursor == NULL)
    { return NULL; }
  else
    { return cursor->ifa_name; }
}

#ifdef TEST
int main(int argc, char **argv)
{
  char *interface = "eth0";
  int index;

  /* printf("%s -> %s\n", interface, interface_to_ip(interface)); */

  index = 0;
  while (1)
    {
      interface = interface_name(index);
      ++ index;

      if (interface == NULL)
	{ break; }

      printf("%s -> %s\n", interface, interface_to_ip(interface));
    }

  return 0;
}
#endif
