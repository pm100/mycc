# 'Writing a C Compiler' on Windows
This document summarizes a lot of content spread out through my blog working through this book. The blog is here https://jollygoodsw.wordpress.com/2025/03/13/working-through-writing-a-c-compiler/ - there is basically a post per chapter

The book expects you to be working in Unix-like environment:
- Linux
- Mac
- WSL (Windows subsystem for Linux)

This walks through everything needed to work on Windows natively, in some places choices had to be made, I hope I made the best decisions 😀

The changes fall into several sections
- compiler, assembler, linker toolchain
- calling convention differences
- Windows 'long' vs 'long long'
- Test harness 

I had to make extensive changes to the test suite, my fork is here https://github.com/pm100/writing-a-c-compiler-tests, please post issues there if you have problems
## Toolchain

The book uses gcc and clang for
- the suggested preprocessor pass
- compiling some tests in the test framework (many tests involve a pair of c files, one compiled by the WACC compiler, one by the platform compiler, this confirms calling convention correctnes)
- assembling some tests
- the suggested linker for you compiler to use

So for Windows you need 

- a c compiler to act as the preprocessor
- an assembler
- a c compiler for the library call tests
- a linker

I managed to get gcc working on windows but decided that I wanted to use the 'native' compiler, this is Microsoft Visual C, MSVC.

The platform C compiler is also used as the preprocessor. MSVC can be used for this too

The book uses gnu as as the assembler. This is what I ended up doing too

MSVC linker is used

I used IDA free as my assembler level debugger. Other choices (all from Microsoft)
- visual studio IDE
- windbg (simple GUI)
- cdbg (command line, like gdb)

### MSVC as preprocessor

First install Microsoft Visual Studio Community Edition, this is free. https://visualstudio.microsoft.com/vs/community/

All your work will need to be done in a visual studio command line shell (this sets up all the needed paths and environment variables). The MSVC installer will have set this up for you. I recommend using Windows Terminal, if that is installed before installing MSVC then you will have a shortcut to the correct shell in the terminal menu.

To use MSVC as a preprocessor use:

`cl /EP /E -DLONG64="long long" /Fi<output_file_name> source_file_name`

(see later for a discussion of that LONG64 define)

### Assembler

I go into this in a lot of detail in this post https://jollygoodsw.wordpress.com/2025/03/20/working-through-wacc-chapter-9-functions/

The bottom line is that for several reasons I strongly recommend (I could not make it work at all) not using MASM / ML64, the Microsoft assembler that comes with MSVC. I ended up using gnu as. (after abandoning nasm)

Installing gnu assembler
- https://github.com/niXman/mingw-builds-binaries/releases
- download x86_64-15.1.0-release-win32-seh-msvcrt-rt_v12-rev0.7z (or whatever is the latest version)
- extract just one file 'as.exe' and place it somewhere in your path

(Note that if you want to use gcc compiler suite then this is the correct download to use. You just have to extract and set up a lot more)

Command line is

  `as -o <output file> <input file>`

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

What follows are summaries. Read https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170 for full details
The main difference is that when a struct / union is passed or returned from a function.

### Scalar arguments

- First four arguments are passed in registers, note the difference for how XMM and regular registers are used
- The rest are pushed

V important, note the requirement to allocate 32 bytes of stack for the caller to provide it with stack space to store the register arguments if it wants to.

### Struct as argument

- The caller makes a copy of the struct in its stack frame.
- The address of this copy is passed in the same way any pointer would be (in a register or pushed depending on how many arguments there are)
- This means that the calling code is dealing with a pointer, not the actual struct

So this code
```
struct s{
  int x;
}
int callee(struct s sarg){
  return s.x;
}
int caller(){
  struct s s1 = {0};
  callee(s);
}
```

is compiled as

```
struct s{
  int x;
}
int callee(struct s *sarg){
  return s->x;
}
int caller(){
  struct s s1 = {0};
  struct s temp = s1;
  callee(&temp);
}
```

The callee is in a strange situation, syntactically the argument is a struct, semantically its a pointer to a struct. There are a couple of ways to make this work

 - #1 Make the parser phase generate TACKY code that understands this situation and treats these arguments as a special case. This is certainly doable (I did it but decided against it in the end) but it means that this phase now knows about the OS that the backend will target. The whole point of TACKY is to be CPU and OS neutral (and it mainly is)
 - #2 Make the TACKY to assembly fix up phase change all accesses to the arguments to be pointers rather than direct. This will be hard to do, a lot of code needs to be changed
 - #3 Make the callee recopy the argument into its own private copy

I ended up doing #3. Its expensive in time and space, but, no sane c programmer passes structs as arguments to a function, so its an acceptable over head.

So the demo code ends up as 

```
struct s{
  int x;
}
int callee(struct s *sarg){
  struct temp = *sarg;
  return temp.x;
}
int caller(){
  struct s s1 = {0};
  struct s temp = s1;
  callee(&temp);
}
```

### Returning structs

- The caller allocates space for the returned struct in its own stack frame
- the address of the return buffer is passed as the first argument
- all the other arguments are shifted to the left

So this
```
struct s{
  int x;
}
struct s callee(int val){
  struct s ret = {val * 2};
  return ret;
}
int caller(){
  struct s ret = callee(5);
}
```
is compiled as
```
struct s{
  int x;
}
struct s callee(struct s *retbuff, int val){
  struct s ret = {val * 2};
  *retbuff = ret;
}
int caller(){
  struct s retbuff ;
  callee(&retbuff, 5);
}
```
In theory the callee could operate on `retbuff` directly but in practice thats really hard to do.




### Stack alignment

It *must* be 16 byte aligned when you call, very odd errors occur if you dont do that.

## Testing

If you are using the compilers that I suggest then you will need to run the tests in a Visual Studio command prompt in order to have the compiler correctly configured
The check-setup option will verify that msvc and as can both be found.

Windows does not support 'shebang' scripts so you have to invoke the tests like this

` python .\test_compiler C:\work\mycc\target\debug\mycc.exe --chapter 19   --verbose    --extra-credit --failfast --latest`

Changes made to the tests
- replace all 'long' by 'LONG64'
- added windows assembler files for the tests that use assembler
- minor tweaks to a few other tests

Changes to the test engine
- fix file names for executables, 'exe' instand of nothing
- for object files use 'obj' instead of 'o'
- deal with \ vs / in file paths

### Using this fork on linux
These tests will not work unless you include a define of LONG64 in your preprocsser invokation.

Like this

```
    fn msvc(path: &Path, source: &Path, dest: &Path) -> Result<()> {
        capture({
            Command::new(path)
                .args(["/EP", "/P", format!("/Fi{}", dest.display()).as_str()])
                .arg("/DLONG64=long long")
                .arg(source)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?
        })?;
        Ok(())
    }
```

(My rust code to invoke msvc as the preprocssor before running my compiler pass)