#include <stdio.h>

int main(){
int a = 10;
float b = 3.14159;
char c = 'A';

printf( "%05x\n",a);
printf( "%.2f\n",b);
printf( "%c = %d\n", c, c);

return 0;
}

char* my_strstr(char *str1, char *str){
    unsigned int count = 0;
    char * last_start = NULL;

    while(*str1 != '\0'){
        char *start = str1;
        char *pattern = str;

        while(*str1 == *pattern && *pattern != '\0'){
            str1++;
            pattern++;
        }

        if(*pattern == '\0'){
            count++;
            last_start = start;
        }

        str1 = start + 1;
    }

    printf("%d\n", count);

    return last_start;
}