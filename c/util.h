// I'm no C expert (so there could be a better way or tools to make
// the process easier) but it feels like C involves a decent amount of
// manual work if you want to break up code into different
// packages/modules/files. For every ".c" library file you should
// create a corresponding ".h" file, called a "header" file. In short
// this file should contain the API for your library. This means it
// contains C declarations (functions and variables), type
// definitions, other #includes (so that any ".c" file which
// #include's this header file will compile. I think this means you
// include any header files which define types that your header file
// uses), macros, and maybe some inline functions. Any code which
// wants to use your library will #include it's header file. The
// effect of doing this #include is that those declarations will be
// visible in your code and you can reference the library's
// functions/variables/types without getting compiler errors. Then
// when the code is linked you specify your code and the library it
// uses and the linker will take care of assigning definitions to the
// declarations. For example with gcc you can do: gcc main.c
// library.c.

// In other languages what is/is not part of your API typically lives
// within the source code itself (capital letters for Go,
// public/private for Java, etc...), but this is not the case with C
// and I am not sure why. Perhaps that sort of logic was too hard to
// code back in the early days of programming? Or maybe this was done
// for compilation performance improvements (e.g. if file A includes
// file B and C's header files you only need to compile A once even if
// B and C change because A does not really care too much about the
// "definition" of things, it only needs their declarations and the
// linker will take care of associating those declarations with
// definitions)? Or perhaps there is some other benefit to doing
// things this way? I'm not really sure, but it definitely seems like
// more work to maintain (not to mention it is error prone) which I
// don't like.

// It's worth noting that there is no real difference between a ".c"
// file and a ".h" file i.e. you can put the same code constructs
// inside either one. The only difference is in how they are
// treated/thought of/used. You also CAN #include one ".c" file inside
// another (since the #include LITERALLY just copy pastes whatever
// file you give it in place of the #include) but my brief research
// seems to indicate that this is bad practice.

// When including header files you have created enclose the header
// file in double quotes (so #include "myheader.h"). This instructs
// the compiler to look for that header file relative to the file
// being compiled.

// /usr/include contains header files for the system. Look through
// some to get a sense of what "standard" header files look like.

// Another bad thing about header files is that it is not at all
// obvious where particular functions/types come from. This problem
// becomes worse when you realize that header files can include other
// header files. For example, say header file A includes stdbool.h and
// source file B includes A and does NOT include stdbool.h and B makes
// use of the "bool" type. If A realizes that it no longer needs
// stdbool.h and removes it, B will now not compile because the "bool"
// type is not defined. I bet in practice this does not come up too
// much and it would be easy to remedy but if you want to write really
// battle hardened code you need to be very aware of where all the
// declarations are coming from and #include them which is too bad.
// This is another thing that I feel like should be handled
// automatically.

// https://www.gamedev.net/articles/programming/general-and-gameplay-programming/organizing-code-files-in-c-and-c-r3173 (or the original post is here: https://www.gamedev.net/articles/programming/general-and-gameplay-programming/organizing-code-files-in-c-and-c-r1798)
// http://www.cplusplus.com/forum/articles/10627/
// https://gcc.gnu.org/onlinedocs/cpp/Header-Files.html

//////////////////////////////////////////////////////////////////////

// These are "include guards" (or "inclusion guards" or "header
// guards" etc...) which check if the header file has been included
// before and if so then don't include it again because that could
// lead to conflicts (e.g. if a header file XYZ defines a type and
// gets included multiple times then the type would be defined
// multiple times and you'd have a conflict). They are necessary
// because the C compiler is dumb and does not attempt to keep track
// of which header files have been included before. The fact that this
// is (pretty much always) necessary but the compiler doesn't take
// care of it for you is a perfect example of manual work that C makes
// you do. It's not hard to do but it sucks that you have to do it at
// all and if you aren't aware of it it can lead to problems. The name
// of the include guard can be anything but typically it is
// "<file-name>_H".
#ifndef UTIL_H
#define UTIL_H

// Include any other header files necessary for this header file.
#include <stdbool.h>		/* Defines the "bool" type */
#include <stddef.h>		/* Defines the "size_t" type */

// An example of a function declaration (also called a function
// prototype).
void *mustMalloc(size_t size);

// A new type.
typedef struct {
  char x;
  char y;
} dir;

// When declaring a variable you have to add the word "extern" to it.
// Typically we think of "int myVar;" as "declaring" the variable but
// in C this also "defines" it. I think the real difference between
// "declaring" and "defining" is that "declaring" means you know what
// the types are and "defining" means that memory was allocated for
// the symbol. If you do "int myVar;" then myVar is "declared" since
// you know it's type but it is also "defined" since memory was
// allocated for it. But prepending "extern" like "extern int myVar"
// makes it so "myVar" is only declared and not defined. By default
// you do not need to put "extern" for function prototypes. In this
// particular case it would probably be better for the application
// itself to define these constants (in general if you have a global
// variable you should think hard about if its really necessary
// because a lot of times it is not) but I'll keep them here for
// pedagogical reasons. More on "extern" here:
// http://www.geeksforgeeks.org/understanding-extern-keyword-in-c/.
extern const dir UP;
extern const dir DOWN;
extern const dir LEFT;
extern const dir RIGHT;
extern const dir PAUSED;

bool dir_equal(dir a, dir b);

typedef struct {
  char x;
  char y;
} pos;

typedef struct {
  int len;
  pos *list;
} posList;

pos createFood(int width, int height, posList snake);
void updateGameState(int width, int height, posList *snake, pos *food, dir curDirection, int *score);
bool gameIsOver(int width, int height, posList snake);
bool gameIsWon(int width, int height, posList snake);

// The end of the include guard. Conventionally you include a comment
// indicating the name of the macro guard that was defined.
#endif /* UTIL_H */
