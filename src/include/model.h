#pragma once

#include <stdbool.h>
#include <GL/glew.h>
#include <SDL2/SDL.h>


#define MAX_VAOS 65536
#define MAX_VBOS 65536
#define MAX_SHADERS 65536
#define MAX_PROGRAMS 65536

typedef struct {
    bool vaos[MAX_VAOS];
    bool vbos[MAX_VBOS];
    bool shaders[MAX_SHADERS];
    bool programs[MAX_PROGRAMS];

    SDL_Window* window;
} Model;
