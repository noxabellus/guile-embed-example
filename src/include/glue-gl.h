#include <GL/glew.h>


#define SHADER_INFO_LOG_LENGTH 512
extern char SHADER_INFO_LOG[SHADER_INFO_LOG_LENGTH];

GLuint glMakeShader (GLenum type, const char* source);
GLuint glMakeProgram (size_t count, GLuint* shaders);


void init_glue_gl();