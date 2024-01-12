#version 330 core

layout (location = 0) in vec2 aPos;

uniform vec2 uResolution;
uniform vec2 uLocation;

void main() {
   vec2 offset = aPos + uLocation;
   vec2 clipSpace = vec2(offset.x, uResolution.y - offset.y) / uResolution * 2.0 - 1.0;
   gl_Position = vec4(clipSpace.x, clipSpace.y, 0.0, 1.0);
}