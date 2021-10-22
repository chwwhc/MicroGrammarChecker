/*
#include<stdio.h>
#include<stdlib.h>

#include "wordlist.h"
*/




int main(int argc, char* argv[]){
    int error_code = 0;
    FILE* file;
    char* input_file = argv[0];

    if(argc == 1){
        printf("Error: Expected at least one argument");
        return 1;
    }

    for(int i = 1; i < argc; i++){
        file = fopen(argv[i], "r");
        if(file == NULL){
            printf("Error: unable to open ");
            printf(argv[i]);
            error_code = 2;
        }
    }

    
    return error_code;
}