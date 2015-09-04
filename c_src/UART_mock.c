#include <stdio.h>
#include <unistd.h>			//Used for UART
#include <fcntl.h>			//Used for UART
#include <termios.h>		//Used for UART
#include <time.h>
#include <string.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <pthread.h>
#define MSG_SIZE 100

// Mocked C Driver - Program echo
 

int packet_n = 0;
int line_n = 0;
pthread_t tid;
int packet_flag = 0;

// File descriptors
int erl_read = 0;
int erl_write = 1;




//SIGPIPE is send when reading end of the pipe gets closed, hence because Erlang's process termination.
int catch(int sig){
	perror("Closing with reason:");
	exit(1);
}

// Termination Function
void close_driver(){
	perror("Closing with reason:");
	exit(1);
}


int main(int argc, char * argv[]){

 struct sigaction sig;
  /* Set up the structure to specify the new action. */
  sig.sa_handler = catch;
  sigemptyset (&sig.sa_mask);
  sig.sa_flags = 0;
 if(sigaction (SIGPIPE, &sig, 0)==-1){
 	printf("Error while sigaction\n");
 	exit(1);
 }



	if(argc<2){
		exit(1);
	}
	if(strcmp(argv[1],"packet")==0){
		packet_n = atoi(argv[2]);
		packet_flag = 1;
	}else{
		packet_flag = 0;
		line_n = atoi(argv[2]);
	}


	while(1){
		unsigned char rx_buffer[MSG_SIZE];
		unsigned char rx_echo[MSG_SIZE];
		int rx_length = read(erl_read, (void*)rx_buffer, sizeof(rx_buffer));		//Filestream, buffer to store in, number of bytes to read (max)
		if (rx_length < 0)
		{
			exit(1);
            
		}else if(rx_length==0){
		exit(1);
		}
		else
		{
			int count_erl;
			strcpy(rx_echo,rx_buffer);
			count_erl = write(erl_write,rx_echo,rx_length);
			if(count_erl <=0){
				close_driver();
			}
		}
		
}
return 0;
}