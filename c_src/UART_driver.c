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

 //C 'Driver' to handle serial communication with UART.



int packet_n = 0;
int line_n = 0;
pthread_t tid;
int packet_flag = 0;

// File descriptors
int uart_fd = -1;
int erl_read = 0;
int erl_write = 1;




//SIGPIPE is send when reading end of the pipe gets closed, hence because Erlang's process termination.
int catch(int sig){
	close(uart_fd);
	perror("Closing with reason:");
	exit(1);
}

// Termination Function
void close_driver(){
	close(uart_fd);
	perror("Closing with reason:");
	exit(1);
}

// pThread which reads packets from erlang
void from_erlang_pthread(){
	char readed[MSG_SIZE];
	char echo[MSG_SIZE];
	while(1){

	int count = read(erl_read, &readed,sizeof(readed));
	if(count<=0){
		close_driver();
	}
	// readed array contains one byte for msg length and rest for content
	strcpy(echo,readed);


	 int count_uart= write(uart_fd, &echo[0], count);		//Filestream, bytes to write, number of bytes to write
		if (count_uart <= 0)
		{
		close_driver();
		}
	
}
}


int main(int argc, char * argv[]){

 struct sigaction sig;
  // Set up SIGPIPE handler
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
	
	
	//OPEN THE UART
	//The flags (defined in fcntl.h):
	//	Access modes (use 1 of these):
	//		O_RDONLY - Open for reading only.
	//		O_RDWR - Open for reading and writing.
	//		O_WRONLY - Open for writing only.
	//
	//	O_NDELAY / O_NONBLOCK (same function) - Enables nonblocking mode. When set read requests on the file can return immediately with a failure status
	//											if there is no input immediately available (instead of blocking). Likewise, write requests can also return
	//											immediately with a failure status if the output can't be written immediately.
	//
	//	O_NOCTTY - When set and path identifies a terminal device, open() shall not cause the terminal device to become the controlling terminal for the process.
	uart_fd = open("/dev/ttyAMA0", O_RDWR | O_NOCTTY );		//Open in non blocking read/write mode

	if (uart_fd == -1)
	{
		//ERROR - CAN'T OPEN SERIAL PORT
		printf("Error - Unable to open UART.  Ensure it is not in use by another application\n");
		exit(1);
	}
	// Set all options to connection
	struct termios options;
	tcgetattr(uart_fd, &options);
	options.c_cflag = B9600 | CS8 | CLOCAL | CREAD;		//<Set baud rate
	options.c_iflag = IGNPAR;
	options.c_oflag = 0;
	options.c_lflag = 0;
	tcflush(uart_fd, TCIFLUSH);
	tcsetattr(uart_fd, TCSANOW, &options);



	if(pthread_create(&tid,NULL,&from_erlang_pthread,NULL)==-1){
		close_driver();
	}

	while(1){

		unsigned char rx_buffer[MSG_SIZE];
		unsigned char rx_echo[MSG_SIZE];
		int rx_length = read(uart_fd, (void*)rx_buffer, sizeof(rx_buffer));		//Filestream, buffer to store in, number of bytes to read (max)
		if (rx_length <= 0)
		{
		close_driver();
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