#include <stdio.h>
#include <string.h>

//typedef int boolean;
//const boolean true = 1, false = 0;

struct studentRecord
{
    char name[100];
    char ip[50];
};

struct studentRecord readLine(char* line)
{
    struct studentRecord sr;
    char* line_name = NULL;
    char* line_ip = NULL;
    line_name = strtok(line, ",");
    strcpy(sr.name, line_name);
    for(int i = 0; i < 6; i++){
        line_ip = strtok(NULL, ",");
    }
    strcpy(sr.ip, line_ip);
    return sr;
}

boolean checkNameExists(FILE* csv, char* name, char* ip){
    char logline[200];
    struct studentRecord sr;

    while(!feof(csv)){
        fgets(logline, 200, csv);
        if(feof(csv)){
            break;
        }

        sr = readLine(logline);
        if(strcmp(sr.name, name) == 0){
            strcpy(ip, sr.ip);
            return true;
        }
    }
    return false;
}

boolean findCollaborators(char *studentname, char *studentip, FILE *csv, FILE *wfile){
    char logline[200];
    struct studentRecord sr;
    boolean found = false;
    char prevname[100] = {'\0'};


    
    while(!feof(csv)){
        fgets(logline, 200, csv);
        if(feof(csv)){
            break;
        }

        sr = readLine(logline);
        if(strcmp(sr.ip, studentip) == 0 && strcmp(sr.name, studentip) != 0 && strcmp(sr.name, prevname) != 0){
            found = true;
            strcpy(prevname, sr.name);
            fprintf(wfile, "%s\n", sr.name);
        }
    }
    return found;
}

int main(int argc, char* argv[]){
    char logline[200], studentname[100], studentip[50];
    FILE *csv, *wfile;
    boolean found;

    if(argc != 4){
        fprintf(stderr, "Usage ./report <csvfile> \"<student name>\" <reportfile>\n");
        return 1;
    }

    csv = fopen(argv[1], "rt");
    if(csv == NULL){
        fprintf(stderr, "Error, unable to open csv file %s\n", argv[2]);
        return 1;
    }

    fgets(logline, 200, csv);
    if(checkNameExists(csv, argv[2], studentip) == false){
        fprintf(stderr, "Error, unable to locate %s\n", argv[2]);
        return 1;
    }

    wfile = fopen(argv[3], "wt");
    if(wfile == NULL){
        fprintf(stderr, "Error, unable to open the output file %s\n", argv[3]);
        return 1;
    }

    rewind(csv);
    fgets(logline, 200, csv);

    found = findCollaborators(argv[2], studentip, csv, wfile);
    if(!found){
        fprintf(wfile, "No collaboratoes found for %s\n", argv[2]);
    }

    fclose(csv);
    fclose(wfile);
    return 0;
}


