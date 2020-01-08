#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <signal.h>
#include <netinet/in.h>
#include <unistd.h>
#include <sys/socket.h>
#include <string.h>
#include <netdb.h>

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

short server_port = htons(18880);

int sock = 0;

char buf[200];
char auth[64];
char res[87];
int count = 0;

int io[2];
int pid = 0;
char l_ip[] = "10.253.0.  ";
char r_ip[] = "10.253.0.  ";

bool handshaken1 = false, handshaken2 = false, first = true;


void load_icon()
{
	struct sockaddr_in serv_addr, clnt_addr;
	struct hostent *hp;
	
	int sock2 = socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
	bzero(&clnt_addr, sizeof(clnt_addr));
	clnt_addr.sin_family = AF_INET;
	clnt_addr.sin_addr.s_addr = INADDR_ANY;
	bind(sock2, (struct sockaddr*)&clnt_addr, sizeof(clnt_addr));

	bzero(&serv_addr, sizeof(serv_addr));
	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons(80);
	hp = gethostbyname("194.88.210.5");
	memcpy((char*)&serv_addr.sin_addr, hp->h_addr, hp->h_length);
	connect(sock2, (sockaddr*)&serv_addr, sizeof(sockaddr_in));	
	
	char to_send[] = "GET /icons/google.gif HTTP/1.1\nAccept: */*\nAccept-Language: ru\nUA-CPU: x86\nAccept-Encoding: gzip, deflate\nIf-Modified-Since: Wed, 25 Sep 2002 12:01:37 GMT\nUser-Agent: Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1)\nHost: 194.88.210.5\nConnection: Keep-Alive\n\n";
	char to_send2[] = "GET /icons/yandex.gif HTTP/1.1\nAccept: */*\nAccept-Language: ru\nUA-CPU: x86\nAccept-Encoding: gzip, deflate\nIf-Modified-Since: Wed, 25 Sep 2002 12:01:37 GMT\nUser-Agent: Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1)\nHost: 194.88.210.5\nConnection: Keep-Alive\n\n";
	
	send(sock2, to_send, sizeof(to_send), 0);
	send(sock2, to_send2, sizeof(to_send2), 0);
	
	close(sock2);
}


void setup_connection()
{
	handshaken1 = false;
	handshaken2 = false;
	first = true;
	bzero(auth, 64);
	
	struct sockaddr_in serv_addr, clnt_addr;
	struct hostent *hp;
	
	sock = socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
	int sopt = 1;
	setsockopt(sock,SOL_SOCKET,SO_REUSEADDR, &sopt, sizeof(sopt));
	bzero(&clnt_addr, sizeof(clnt_addr));
	clnt_addr.sin_family = AF_INET;
	clnt_addr.sin_addr.s_addr = INADDR_ANY;
	bind(sock, (struct sockaddr*)&clnt_addr, sizeof(clnt_addr));

	bzero(&serv_addr, sizeof(serv_addr));
	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = server_port;
	hp = gethostbyname("3.3.3.1");
	memcpy((char*)&serv_addr.sin_addr, hp->h_addr, hp->h_length);
	printf("connecting...\n");
	connect(sock, (sockaddr*)&serv_addr, sizeof(sockaddr_in));	
	send(sock, "\0\7<init/>", 9, 0);
	printf("awaiting initialization...\n");
	
	count = 0;
}


int main(int argc, char* argv[])
{
	res[0] = 0;
	res[1] = 85;
	memcpy(res+2, "<handshake-ack id='0'><reply>", 29);
	memcpy(res+63, "</reply></handshake-ack>", 24);
	
	while(1)
	{
		while(!handshaken1 || !handshaken2)
		{
			int len = recv(sock, buf, 200, 0);
			write(1, buf, len); printf("\n");
			
			if(len <= 0)
			{
				printf("connection lost, restarting...");				
				
				if(pid)
					kill(pid, SIGKILL);
				setup_connection();
			}
			// recieved Init information
			if(len == 130)
			{
				handshaken1 = true;
				
				memcpy(auth, buf+18, 32);
				memcpy(l_ip+9, buf+72, 2);
				memcpy(r_ip+9, buf+105, 2);
				
				pid = fork();
				if(!pid)
				{
					execlp("openvpn", "openvpn", "--dev", "tun0", "--tun-mtu", "1200", "--proto", "tcp-client", "--ifconfig", l_ip, r_ip, "--remote", "3.3.3.1", "18881", "--uuid", auth, NULL);
					printf("Error: loading openvpn\n");
					return 0;
				}
				if(!fork())
				{
					sleep(5);
					execlp("route", "route", "add", "default", "dev", "tun0", NULL);
					printf("Error: loading route\n");
					return 0;
				}
				
				printf("initial information received\n");
			}
			// received Handshake information
			else if(len == 96)
			{
				handshaken2 = true;
				
				memcpy(auth+32, buf+45, 32);				
			}
			// received Show-Popup information
			else if(first)
			{
				first = false;
				
				load_icon();				
			}
		}
		
		// calculating md5
		pipe(io);
		
		int fd = creat("./md5", O_TRUNC);
		write(fd, auth, 64);
		close(fd);
		
		if(!fork())
		{
			//MD5SUM
			//dup2(io[0], 0);
			dup2(io[1], 1);
			execlp("md5sum", "md5sum", "md5", NULL);
			printf("Error: loading md5sum\n");
			return 0;
		}
		sleep(1);
		read(io[0], res+31, 32);
		
		// sending Handshake reply
		send(sock, res, 0x57, 0);
		printf("%.4d handshake replied\n", count++);
		
		handshaken2 = false;
	}
	
	return 0;
}
