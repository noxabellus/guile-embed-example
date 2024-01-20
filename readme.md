# Guile Embed Example

This is a simple(-ish) example embedding Guile Scheme into a program running SDL2 & OpenGL (via glew)


## Usage
Build and run the program:
```sh
./meta.scm build-and-run scripts/hello.scm
```
+ Press Ctrl + R to hot-reload Guile/OpenGL shader code
+ Press Ctrl + W to enable/disable automatic hot-reload when the Guile file is modified
    > Passing the flag `--auto-reload` after the script path will enable this by default

Configuration for meta can be found in `config.scm`

More commands for meta can be found by running:
```sh
./meta.scm help
```

## Dependencies
```sh
sudo apt install libsdl2-dev libglew-dev guile-3.0 guile-3.0-dev
```

## Details

### Glue
OpenGL <-> Guile glue is in `glue/gl.c`

SDL <-> Guile glue is in `glue/sdl.c`

These are brought together in `glue/bind.c`

Simple helper macros for this section are defined in `include/glue/glueing-utils.h`

### C code
`main.c` creates a window using SDL, sets up the OpenGL context, and then initializes Guile with the glue code builder function. It then executes the script provided as a command line argument (`scripts/hello.scm` is provided/explained here) with an exception handler which simply prints the exception and exits, for now

The script is allowed to perform any setup needed, and is expected to define two procedures called `update` and `render`, which are then called in a loop by `main.c` (again, in an exception handler) until the program is exited

During the event loop, SDL keyup is checked for Ctrl + R, in which case the model is reset and the guile code is reloaded

### Guile code
On initialization, `hello.scm` creates a vertex buffer, vao, uniforms, and simple shader program with a source loaded via Guile ports, binding these with the glue code

In `update`, the script gets the SDL window size with the glue code, and calculates updated values for cpu-side versions of uniforms in a simple way

In `render`, the script binds the program and vao, updates the OpenGL uniforms, then calls `gl:draw-triangles`
