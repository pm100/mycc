# 'Writing a C Compiler' on Windows
This document summarizes a lot of content spread out through my blog working through this book. The blog is here https://jollygoodsw.wordpress.com/2025/03/13/working-through-writing-a-c-compiler/ - there is basically a post per chapter

The book expects you to be working in Unix-like environment:
- Linux
- Mac
- WSL (Windows subsystem for Linux)

This document walks through everything needed to work on Windows natively, in some places choices had to be made, I hope I made the best decisions 😀

The changes fall into several sections
- compiler, assembler, linker toolchain
- calling convention differences
- Windows 'long' vs 'long long'
- Test harness 

I had to make extensive changes to the test suite, my fork is here https://github.com/pm100/writing-a-c-compiler-tests
## Toolchain

The book uses gcc and clang when compiling code to test your compiler (many tests involve a pair of c files, one compiled by the WACC compiler, one by the platform compiler, this confirms calling convention correctness). For Windows you need to use Microsoft Visual Studio C Compiler - MSVC from now on.

The platform C compiler is also used as the preprocessor. MSVC can be used for this too

The book uses gas as the assembler. I chose nasm

MSVC linker is used

I used IDE free as my assembler level debugger. 

### MSVC as preprocessor

First install Microsoft Visual Studio Community Edition, this is free. https://visualstudio.microsoft.com/vs/community/

All your work will need to be done in a visual studio command line shell (this sets up all the needed paths and environment variables). The MSVC installer will have set this up for you. I recommend using Windows Terminal, if that is installed before installing MSVC then you will have a shortcut to the correct shell in the terminal menu.

To use MSVC as a preprocessor use:

`cl /EP /E -DLONG64="long long" /Fi<output_file_name> source_file_name`

(see later for a discussion of that LONG64 define)

### Assembler

I go into this ina lot of detail in this post https://jollygoodsw.wordpress.com/2025/03/20/working-through-wacc-chapter-9-functions/

The bottom line is that for several reasons I strongly recommend (I could not make it work at all) not using MASM / ML64, the Microsoft assembler that comes with MSVC. I ended up using NASM https://www.nasm.us/. 

Command line is

  `nasm -f win64 -o <output file> <input file>`

nasm, like masm , uses a different assembler syntax from gas (and hence what the book uses)

- source and dest are reversed. nasm uses `mov rax,42` (think 'let rax = 42')
- there are no instruction size qualifiers like movb, movq,.. instead the size is inferred from the size of the operands
- misc other changes to various directives.

To get a feel for nasm syntax here is the code my compiler generates for this c code

```
int main(void) {
    int x = 42;
    int y = 24;
    return x+y;
}
```

```
bits 64
default rel
segment .text
global main
main: 
        push rbp
        mov rbp, rsp
        sub rsp, 16
        mov DWORD [rbp-4], 42
        mov DWORD [rbp-8], 24
        mov r10d, DWORD [rbp-4]
        mov DWORD [rbp-16], r10d
        mov r10d, DWORD [rbp-8]
        add DWORD [rbp-16], r10d
        mov eax, DWORD [rbp-16]
        mov rsp, rbp
        pop rbp
        ret
        mov eax, 0
        mov rsp, rbp
        pop rbp
        ret
```
### Linker

Use the MSVC linker - called link! It needs to be told what system libraries to include. Command line is

`link /subsystem:console /entry:main /defaultlib:ucrt.lib /defaultlib:msvcrt.lib /defaultlib:libvcruntime.lib /defaultlib:legacy_stdio_definitions.lib /defaultlib:Kernel32.lib /defaultlib:Shell32.lib /nologo  /incremental:no <input file(s)> /out:<output file>`

## Windows long vs long long

Discussed at length here https://jollygoodsw.wordpress.com/2025/04/06/working-through-wacc-chapter-11-long-integers/

MSVC uses LLP64 format for 64 bit, see https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models. The bottom line is that `long` in MSVC is 32-bit, `long long` is 64 bit.

I therefore changed all of the test programs and replaced `long` with `LONG64` and then depending on the platform defined that macro either
- `-DLONG64=long` on mac and linux
- `-DLONG64=long long` on windows

I was surprised that I didn't need to change `42l` to `42ll`. 

## Calling convention
The windows x64 calling convention is different from the Unix one. In a few places its very different.
References:
- https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170