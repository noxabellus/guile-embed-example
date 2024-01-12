# Guile Embed Example

This is a simple(-ish) example embedding Guile Scheme into a program running SDL2 & OpenGL (via glew)

## Details
OpenGL <-> Guile glue is in `glue-gl.c` (simple helper macros for this are defined in `glueing-utils.h`)

`main.c` creates a window using SDL, sets up the OpenGL context, and then executes the script `scripts/hello.scm` with an exception handler (that simply prints the exception and exits, for now)

The script is allowed to perform any setup needed, and is expected to define a procedure called `render` which is then called in a loop by `main.c` (again, in an exception handler) until the program is exited

`hello.scm` simply creates a vertex buffer, vao, and simple shader program with a source loaded via Guile ports, binding these with the glue code; and in `render` it just binds the program and vao, then calls `gl:draw-triangles`

## Dependencies
```sh
sudo apt install libsdl2-dev libglew-dev guile-3.0 guile-3.0-dev
```

## Usage
Build and run the program:
```sh
./meta.scm build-and-run
```
More info:
```sh
./meta.scm help
```