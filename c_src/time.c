#include <time.h>
#include <stdio.h>
void format_time(char * output){
    time_t rawtime;
    struct tm * timeinfo;

    time ( &rawtime );
    timeinfo = localtime ( &rawtime );

    sprintf(output, "%02d/%02d/%02d %02d:%02d:%02d",timeinfo->tm_mday, timeinfo->tm_mon + 1, timeinfo->tm_year + 1900, timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec);
}
int main(void)
{
char buffer[20];
 format_time(buffer);
 printf("%s",buffer);
    return 0;
}