# How to use:
1. Put all the files in the same directory.
2. Run the C preprocessor on the file that you want to check. (using `cpp xxx.c xxx.c` or `gcc -E xxx.c >> xxx.c`)
3. Run `ghci` on the command line.
4. Enter command `:l checker`.
5. Enter command `checkFile`.
6. Enter the name of the file that you want to check
7. If the checker successes, you will be prompted with "checking completed". You can then find the report in the generated `report.txt` file in the working directory.

# Video Demo:
https://youtu.be/6FZqs9Un-1U
