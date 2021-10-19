//#include <stdio.h>

/* Name: Hanwen Zhu
   ID: 260838344
*/

/*
Program to implement a scientific calculator
**************************************************************
* Author      Dept.                Date          Notes
**************************************************************
* Hanwen Z    Comp. Science.    Nov 4 2020    Initial version
* Hanwen Z    Comp. Science.    Nov 6 2020    Added comments
*/



int main(int argc, char *argv){
int count=0;
char result;

    //Initialize result as a char array containing only 0
    for(count = 0;count <=999; count++){
        result[count] = 48;
    }

    //Check if there are exactly 3 inputs
    if(argc != 4){
        printf("Error: invalid number of arguments!\n");
        printf("scalc <operand1> <operator> <operand2>\n");
        return 1;
    } 
    //Check if the second input is "+"
    if(argv[2] != 43){
        printf("Error: operator can only be + !\n");
        return 1;
    }
    //Check if the remaining inputs are positive integers
    for(count = 0; argv[count] != '\0'; count++){
        if((argv[count] > 57) || (argv[count] < 48)){
          printf("Error!! operand can only be positive integers\n");
          return 1;
        }
    }
    for(count = 0; argv[count] != '\0'; count++){
        if((argv[count] > 57) || (argv[count] < 48)){
          printf("Error!! operand can only be positive integers\n");
          return 1;
        }
    }

    //Determine the size of the 2 operands
    for(count = 0; argv[count] != '\0'; count++){
        sizeOf1++;
    }
    sizeOf1--;
    for(count = 0; argv[count] != '\0'; count++){
        sizeOf2++;
    }
    sizeOf2--;
    //Determine the operand with bigger size
    //Determine the difference between the sizes
    if(sizeOf1 > sizeOf2){
        sizeDiff = sizeOf1 - sizeOf2;
        biggerOne = 1;
    } else if(sizeOf1 < sizeOf2){
        sizeDiff = sizeOf2 - sizeOf1;
        biggerOne = 2;
    } else {
        biggerOne = 0;
    }

    //Compute the final result
    if(biggerOne == 1){
        for(sizeOf2; sizeOf2 >= 0; sizeOf2--){
            intermediate = argv[sizeOf1-shift] + argv[sizeOf2] + result[sizeOfResult-shift];
            if(intermediate > 153){
                result[sizeOfResult - shift] = intermediate - 106;
                result[sizeOfResult - shift - 1] = 49;
            } else {
                result[sizeOfResult - shift] = intermediate - 96;
            }
            shift++;
        }
        sizeOf1 = sizeOf1 - shift;
        for(sizeOf1; sizeOf1 >=0; sizeOf1--){
            intermediate = argv[sizeOf1] + result[sizeOfResult-shift];
            if(intermediate > 105){
                result[sizeOfResult - shift] = intermediate - 58;
                result[sizeOfResult - shift - 1] = 49;
            } else {
                result[sizeOfResult - shift] = intermediate - 48;
            }
            shift++;
        }
    } else if(biggerOne == 2){
        for(sizeOf1; sizeOf1 >= 0; sizeOf1--){
            intermediate = argv[sizeOf1] + argv[sizeOf2-shift] + result[sizeOfResult-shift];
            if(intermediate > 153){
                result[sizeOfResult - shift] = intermediate - 106;
                result[sizeOfResult - shift - 1] = 49;
            } else {
                result[sizeOfResult - shift] = intermediate - 96;
            }
            shift++;
        }
        sizeOf2 = sizeOf2 - shift;
        for(sizeOf2; sizeOf2 >=0; sizeOf2--){
            intermediate = argv[sizeOf2] + result[sizeOfResult-shift];
            if(intermediate > 105){
                result[sizeOfResult - shift] = intermediate - 58;
                result[sizeOfResult - shift - 1] = 49;
            } else {
                result[sizeOfResult - shift] = intermediate - 48;
            }
            shift++;
        }
    } else {
         for(sizeOf1; sizeOf1 >= 0; sizeOf1--){
            intermediate = argv[sizeOf1] + argv[sizeOf1] + result[sizeOfResult-shift];
            if(intermediate > 153){
                result[sizeOfResult - shift] = intermediate - 106;
                result[sizeOfResult - shift - 1] = 49;
            } else {
                result[sizeOfResult - shift] = intermediate - 96;
            }
            shift++;
        }
    }

    //Find the first non-zero digit
    for(count = 0;result[count] != '\0';count++){
        if(result[count] != '0'){
            break;
        }
    }
    //If result only contains 0, print 0
    if(count == 1000){
        putchar('0');
    } 
    //Print the final result
    else{
    while(count < 1000){
        putchar(result[count]);
        count++;
    }
    }
    printf("\n");
    //If successful return 0
    return 0;
}
