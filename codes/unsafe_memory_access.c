#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>

#define CORRECT_VER 0

void test(char* p){
    strcpy(p, "hello"); //exceeds the segment limitation of pointer p (unsafe memory access)
}

int main(){
    #if CORRECT_VER
    char* pp = (char*) malloc(10 * sizeof(char));
    test(pp);
    printf(pp);
    free(pp);
    
    #else
    char* p; //here granted pointer p with no memory space (declare without definition initialization)
    test(p);
    printf(p);
    
    #endif
    return 0;
}