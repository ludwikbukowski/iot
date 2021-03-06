#include <stdio.h>
#include <unistd.h>			//Used for UART
#include <fcntl.h>			//Used for UART
#include <termios.h>		//Used for UART
#include <time.h>
#include <string.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <pthread.h>
#define MSG_SIZE 256
#define MEASURE_LENGTH 5 // RXXX\r is 5 length long
#define DATE_LENGTH 19 // 'YYYY/MM/DD HH:MM:SS'
 //C 'Driver' to handle serial communication with UART.



int packet_n = 0;
int line_n = 0;
pthread_t tid;
int packet_flag = 0;

// File descriptors
int uart_fd = -1;
int erl_read = 0;
int erl_write = 1;
char time_buffer[19];

// Prints Time formated as 'YYYY/MM/DD HH:MM:SS'
void format_time(char * output){
    time_t rawtime;
    struct tm * timeinfo;
    time ( &rawtime );
    timeinfo = localtime ( &rawtime );
    sprintf(output, "%02d/%02d/%02d %02d:%02d:%02d",
    timeinfo->tm_mday, timeinfo->tm_mon + 1, timeinfo->tm_year + 1900, timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec);
}

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
	uart_fd = open("/dev/ttyAMA0", O_RDWR | O_NOCTTY  );		//Open in non blocking read/write mode

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



    fsync(uart_fd);
	while(1){

		unsigned char rx_buffer[MSG_SIZE];
		unsigned char rx_echo[MSG_SIZE+1];
		int rx_length = read(uart_fd, (void*)rx_buffer, sizeof(rx_buffer));		//Filestream, buffer to store in, number of bytes to read (max)
		if (rx_length <= 0)
		{
		close_driver();
		}
		else
		{
			int count_erl;
			format_time(time_buffer);
			rx_echo[0] = MEASURE_LENGTH + DATE_LENGTH;
			strcpy(rx_echo+1,rx_buffer);
			strcpy(rx_echo+1+MEASURE_LENGTH, time_buffer);
			count_erl = write(erl_write,rx_echo,rx_length+1+DATE_LENGTH);
			if(count_erl <=0){
				close_driver();
			}
		}
		
}
return 0;
}