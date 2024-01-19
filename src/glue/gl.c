#include <stdint.h>
#include <stdio.h>
#include <GL/glew.h>
#include <libguile.h>

#include "glue/gl.h"
#include "glue/glueing-utils.h"
#include "model.h"

static
Model* MODEL = NULL;

char SHADER_INFO_LOG[SHADER_INFO_LOG_LENGTH] = {};

GLuint glMakeShader (GLenum type, const char* source) {
    GLuint shader = glCreateShader(type);
    glShaderSource(shader, 1, &source, NULL);
    glCompileShader(shader);

    GLuint success;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (!success) {
        glGetShaderInfoLog(shader, SHADER_INFO_LOG_LENGTH, NULL, SHADER_INFO_LOG);
        return 0;
    }

    return shader;
}

GLuint glMakeProgram (size_t count, GLuint* shaders) {
    GLuint program = glCreateProgram();
    for (int i = 0; i < count; i++) {
        glAttachShader(program, shaders[i]);
    }
    glLinkProgram(program);

    GLuint success;
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if (!success) {
        glGetProgramInfoLog(program, SHADER_INFO_LOG_LENGTH, NULL, SHADER_INFO_LOG);
        return 0;
    }

    return program;
}



SCM glue_glClearColor (SCM r, SCM g, SCM b, SCM a) {
    glClearColor(scm_to_double(r), scm_to_double(g), scm_to_double(b), scm_to_double(a));
    return SCM_UNSPECIFIED;
}

SCM glue_glClear (SCM mask) {
    glClear(scm_to_int(mask));
    return SCM_UNSPECIFIED;
}



SCM glue_glMakeShader (SCM type, SCM source) {
    GLuint shader = glMakeShader(scm_to_int(type), scm_to_utf8_string(source));
    if (shader == 0) scm_throw(scm_from_utf8_symbol("shader-compile-failed"), scm_from_utf8_string(SHADER_INFO_LOG));
    else {
        scm_assert(shader < MAX_SHADERS, "gl-error", "maximum number of shaders reached");
        MODEL->shaders[shader] = true;
        return scm_from_int(shader);
    }
}

SCM glue_glDeleteShader (SCM shader) {
    GLuint ishader = scm_to_int(shader);
    glDeleteShader(ishader);
    scm_assert(MODEL->shaders[ishader], "gl-error", "attempted to delete a non-existent shader");
    MODEL->shaders[ishader] = false;
    return SCM_UNSPECIFIED;
}


SCM glue_glMakeProgram (SCM shaders) {
    size_t count = scm_to_int(scm_length(shaders));
    GLuint shaders_arr[16];
    for (int i = 0; i < count; i++) {
        shaders_arr[i] = scm_to_int(scm_list_ref(shaders, scm_from_int(i)));
    }

    GLuint program = glMakeProgram(count, shaders_arr);
    if (program == 0) scm_throw(scm_from_utf8_symbol("program-link-failed"), scm_from_utf8_string(SHADER_INFO_LOG));
    else {
        scm_assert(program < MAX_PROGRAMS, "gl-error", "maximum number of programs reached");
        MODEL->programs[program] = true;
        return scm_from_int(program);
    }
}

SCM glue_glUseProgram (SCM program) {
    glUseProgram(scm_to_int(program));
    return SCM_UNSPECIFIED;
}

SCM glue_glDeleteProgram (SCM program) {
    GLuint iprogram = scm_to_int(program);
    glDeleteProgram(iprogram);
    scm_assert(MODEL->programs[iprogram], "gl-error", "attempted to delete a non-existent program");
    MODEL->programs[iprogram] = false;
    return SCM_UNSPECIFIED;
}



SCM glue_glGenBuffer () {
    GLuint buffer;
    glGenBuffers(1, &buffer);
    scm_assert(buffer < MAX_VBOS, "gl-error", "maximum number of vbos reached");
    MODEL->vbos[buffer] = true;
    return scm_from_int(buffer);
}

SCM glue_glBindBuffer (SCM target, SCM buffer) {
    glBindBuffer(scm_to_int(target), scm_to_int(buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glBufferData (SCM target, SCM data, SCM usage) {
    scm_t_array_handle handle;
    size_t size;
    size_t len;
    const void* buffer;
    ssize_t inc_p;

    if (scm_f32vector_p(data)) {
        buffer = scm_f32vector_elements(data, &handle, &len, &inc_p);
        size = sizeof(float) * len;
    } else if (scm_u32vector_p(data)) {
        buffer = scm_u32vector_elements(data, &handle, &len, &inc_p);
        size = sizeof(uint32_t) * len;
    } else if (scm_s32vector_p(data)) {
        buffer = scm_s32vector_elements(data, &handle, &len, &inc_p);
        size = sizeof(int32_t) * len;
    } else {
        scm_panic("unsupported-gl-buffer-type", "Only f32, u32, and s32 are supported as of now");
    }

    scm_assert(inc_p == 1, "unsupported-gl-buffer-type", "Only contiguous buffers are supported as of now");

    glBufferData(scm_to_int(target), size, buffer, scm_to_int(usage));

    scm_array_handle_release(&handle);

    return SCM_UNSPECIFIED;
}

SCM glue_glDeleteBuffer (SCM buffer) {
    GLuint ibuffer = scm_to_int(buffer);
    glDeleteBuffers(1, &ibuffer);
    scm_assert(MODEL->vbos[ibuffer], "gl-error", "attempted to delete a non-existent vbo");
    MODEL->vbos[ibuffer] = false;
    return SCM_UNSPECIFIED;
}



SCM glue_glGenVertexArray () {
    GLuint vao;
    glGenVertexArrays(1, &vao);
    scm_assert(vao < MAX_VAOS, "gl-error", "maximum number of vaos reached");
    MODEL->vaos[vao] = true;
    return scm_from_int(vao);
}

SCM glue_glBindVertexArray (SCM vao) {
    glBindVertexArray(scm_to_int(vao));
    return SCM_UNSPECIFIED;
}

SCM glue_glVertexAttribPointer (SCM index, SCM size, SCM type, SCM normalized, SCM stride, SCM offset) {
    glVertexAttribPointer(scm_to_int(index), scm_to_int(size), scm_to_int(type), scm_to_bool(normalized), scm_to_int(stride), (void*)(size_t)scm_to_int(offset));
    return SCM_UNSPECIFIED;
}

SCM glue_glEnableVertexAttribArray (SCM index) {
    glEnableVertexAttribArray(scm_to_int(index));
    return SCM_UNSPECIFIED;
}

SCM glue_glDeleteVertexArray (SCM vao) {
    GLuint ivao = scm_to_int(vao);
    glDeleteVertexArrays(1, &ivao);
    scm_assert(MODEL->vaos[ivao], "gl-error", "attempted to delete a non-existent vao");
    MODEL->vaos[ivao] = false;
    return SCM_UNSPECIFIED;
}



SCM glue_glGetUniformLocation (SCM loc, SCM name) {
    return scm_from_int(glGetUniformLocation(scm_to_int(loc), scm_to_utf8_string(name)));
}


SCM glue_glUniform1f (SCM loc, SCM f0) {
    glUniform1f(scm_to_int(loc), scm_to_double(f0));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform2f (SCM loc, SCM f0, SCM f1) {
    glUniform2f(scm_to_int(loc), scm_to_double(f0), scm_to_double(f1));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform3f (SCM loc, SCM f0, SCM f1, SCM f2) {
    glUniform3f(scm_to_int(loc), scm_to_double(f0), scm_to_double(f1), scm_to_double(f2));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform4f (SCM loc, SCM f0, SCM f1, SCM f2, SCM f3) {
    glUniform4f(scm_to_int(loc), scm_to_double(f0), scm_to_double(f1), scm_to_double(f2), scm_to_double(f3));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform1ui (SCM loc, SCM u0) {
    glUniform1ui(scm_to_int(loc), scm_to_uint(u0));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform2ui (SCM loc, SCM u0, SCM u1) {
    glUniform2ui(scm_to_int(loc), scm_to_uint(u0), scm_to_uint(u1));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform3ui (SCM loc, SCM u0, SCM u1, SCM u2) {
    glUniform3ui(scm_to_int(loc), scm_to_uint(u0), scm_to_uint(u1), scm_to_uint(u2));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform4ui (SCM loc, SCM u0, SCM u1, SCM u2, SCM u3) {
    glUniform4ui(scm_to_int(loc), scm_to_uint(u0), scm_to_uint(u1), scm_to_uint(u2), scm_to_uint(u3));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform1i (SCM loc, SCM u0) {
    glUniform1i(scm_to_int(loc), scm_to_int(u0));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform2i (SCM loc, SCM u0, SCM u1) {
    glUniform2i(scm_to_int(loc), scm_to_int(u0), scm_to_int(u1));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform3i (SCM loc, SCM u0, SCM u1, SCM u2) {
    glUniform3i(scm_to_int(loc), scm_to_int(u0), scm_to_int(u1), scm_to_int(u2));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform4i (SCM loc, SCM u0, SCM u1, SCM u2, SCM u3) {
    glUniform4i(scm_to_int(loc), scm_to_int(u0), scm_to_int(u1), scm_to_int(u2), scm_to_int(u3));
    return SCM_UNSPECIFIED;
}

#define EXTRACT_UNI_BUFFER(type, body) { \
    scm_t_array_handle handle; \
    size_t len; \
    const void* buffer; \
    ssize_t inc_p; \
    if (scm_##type##vector_p(data)) { \
        buffer = scm_##type##vector_elements(data, &handle, &len, &inc_p); \
    } else { \
        scm_throw(scm_from_utf8_symbol("unsupported-gl-buffer-type"), scm_from_utf8_string("Expected an " #type "vector")); \
    } \
    if (inc_p != 1) scm_throw(scm_from_utf8_symbol("unsupported-gl-buffer-type"), scm_from_utf8_string("Only contiguous buffers are supported as of now")); \
    body; \
    scm_array_handle_release(&handle); \
}

SCM glue_glUniform1fv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(f32, glUniform1fv(scm_to_int(loc), len, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform2fv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(f32, glUniform2fv(scm_to_int(loc), len / 2, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform3fv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(f32, glUniform3fv(scm_to_int(loc), len / 3, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform4fv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(f32, glUniform4fv(scm_to_int(loc), len / 4, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform1uiv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(u32, glUniform1uiv(scm_to_int(loc), len, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform2uiv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(u32, glUniform2uiv(scm_to_int(loc), len / 2, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform3uiv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(u32, glUniform3uiv(scm_to_int(loc), len / 3, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform4uiv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(u32, glUniform4uiv(scm_to_int(loc), len / 4, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform1iv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(s32, glUniform1iv(scm_to_int(loc), len, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform2iv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(s32, glUniform2iv(scm_to_int(loc), len / 2, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform3iv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(s32, glUniform3iv(scm_to_int(loc), len / 3, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniform4iv (SCM loc, SCM data) {
    EXTRACT_UNI_BUFFER(s32, glUniform4iv(scm_to_int(loc), len / 4, buffer));
    return SCM_UNSPECIFIED;
}

SCM glue_glUniformMatrix3 (SCM loc, SCM transpose, SCM data) {
    EXTRACT_UNI_BUFFER(f32, {
        if (len != 9) scm_throw(scm_from_utf8_symbol("unsupported-gl-buffer-type"), scm_from_utf8_string("Expected a 3x3 matrix"));
        glUniformMatrix3fv(scm_to_int(loc), 1, scm_to_bool(transpose), buffer);
    });
    return SCM_UNSPECIFIED;
}

SCM glue_glUniformMatrix4 (SCM loc, SCM transpose, SCM data) {
    EXTRACT_UNI_BUFFER(f32, {
        if (len != 16) scm_throw(scm_from_utf8_symbol("unsupported-gl-buffer-type"), scm_from_utf8_string("Expected a 3x3 matrix"));
        glUniformMatrix4fv(scm_to_int(loc), 1, scm_to_bool(transpose), buffer);
    });
    return SCM_UNSPECIFIED;
}

SCM glue_glUniformMatrix3v (SCM loc, SCM transpose, SCM data) {
    EXTRACT_UNI_BUFFER(f32, {
        glUniformMatrix3fv(scm_to_int(loc), len / 9, scm_to_bool(transpose), buffer);
    });
    return SCM_UNSPECIFIED;
}

SCM glue_glUniformMatrix4v (SCM loc, SCM transpose, SCM data) {
    EXTRACT_UNI_BUFFER(f32, {
        glUniformMatrix4fv(scm_to_int(loc), len / 16, scm_to_bool(transpose), buffer);
    });
    return SCM_UNSPECIFIED;
}



SCM glue_glDrawArrays (SCM mode, SCM first, SCM count) {
    glDrawArrays(scm_to_int(mode), scm_to_int(first), scm_to_int(count));
    return SCM_UNSPECIFIED;
}


void bind_gl_glue (void* model) {
    MODEL = model;

    EXPORT_CONSTANT("+float+", scm_from_int(GL_FLOAT));
    EXPORT_CONSTANT("+unsigned-int+", scm_from_int(GL_UNSIGNED_INT));
    EXPORT_CONSTANT("+int+", scm_from_int(GL_INT));

    EXPORT_CONSTANT("+vertex-shader+", scm_from_int(GL_VERTEX_SHADER));
    EXPORT_CONSTANT("+fragment-shader+", scm_from_int(GL_FRAGMENT_SHADER));

    EXPORT_CONSTANT("+array-buffer+", scm_from_int(GL_ARRAY_BUFFER));
    EXPORT_CONSTANT("+element-array-buffer+", scm_from_int(GL_ELEMENT_ARRAY_BUFFER));

    EXPORT_CONSTANT("+stream-draw+", scm_from_int(GL_STREAM_DRAW));
    EXPORT_CONSTANT("+static-draw+", scm_from_int(GL_STATIC_DRAW));
    EXPORT_CONSTANT("+dynamic-draw+", scm_from_int(GL_DYNAMIC_DRAW));

    EXPORT_CONSTANT("+triangles+", scm_from_int(GL_TRIANGLES));

    EXPORT_CONSTANT("+color-buffer-bit+", scm_from_int(GL_COLOR_BUFFER_BIT));
    EXPORT_CONSTANT("+depth-buffer-bit+", scm_from_int(GL_DEPTH_BUFFER_BIT));

    EXPORT_PROCEDURE("clear-color", 4, 0, 0, glue_glClearColor);
    EXPORT_PROCEDURE("clear", 1, 0, 0, glue_glClear);

    EXPORT_PROCEDURE("make-shader", 2, 0, 0, glue_glMakeShader);
    EXPORT_PROCEDURE("delete-shader", 1, 0, 0, glue_glDeleteShader);

    EXPORT_PROCEDURE("make-program", 0, 0, 1, glue_glMakeProgram);
    EXPORT_PROCEDURE("use-program", 1, 0, 0, glue_glUseProgram);
    EXPORT_PROCEDURE("delete-program", 1, 0, 0, glue_glDeleteProgram);

    EXPORT_PROCEDURE("gen-buffer", 0, 0, 0, glue_glGenBuffer);
    EXPORT_PROCEDURE("bind-buffer", 2, 0, 0, glue_glBindBuffer);
    EXPORT_PROCEDURE("buffer-data", 3, 0, 0, glue_glBufferData);
    EXPORT_PROCEDURE("delete-buffer", 1, 0, 0, glue_glDeleteBuffer);

    EXPORT_PROCEDURE("gen-vertex-array", 0, 0, 0, glue_glGenVertexArray);
    EXPORT_PROCEDURE("bind-vertex-array", 1, 0, 0, glue_glBindVertexArray);
    EXPORT_PROCEDURE("vertex-attrib-pointer", 6, 0, 0, glue_glVertexAttribPointer);
    EXPORT_PROCEDURE("enable-vertex-attrib-array", 1, 0, 0, glue_glEnableVertexAttribArray);
    EXPORT_PROCEDURE("delete-vertex-array", 1, 0, 0, glue_glDeleteVertexArray);

    EXPORT_PROCEDURE("get-uniform-location", 2, 0, 0, glue_glGetUniformLocation);

    EXPORT_PROCEDURE("uniform1f", 2, 0, 0, glue_glUniform1f);
    EXPORT_PROCEDURE("uniform2f", 3, 0, 0, glue_glUniform2f);
    EXPORT_PROCEDURE("uniform3f", 4, 0, 0, glue_glUniform3f);
    EXPORT_PROCEDURE("uniform4f", 5, 0, 0, glue_glUniform4f);
    EXPORT_PROCEDURE("uniform1ui", 2, 0, 0, glue_glUniform1ui);
    EXPORT_PROCEDURE("uniform2ui", 3, 0, 0, glue_glUniform2ui);
    EXPORT_PROCEDURE("uniform3ui", 4, 0, 0, glue_glUniform3ui);
    EXPORT_PROCEDURE("uniform4ui", 5, 0, 0, glue_glUniform4ui);
    EXPORT_PROCEDURE("uniform1i", 2, 0, 0, glue_glUniform1i);
    EXPORT_PROCEDURE("uniform2i", 3, 0, 0, glue_glUniform2i);
    EXPORT_PROCEDURE("uniform3i", 4, 0, 0, glue_glUniform3i);
    EXPORT_PROCEDURE("uniform4i", 5, 0, 0, glue_glUniform4i);

    EXPORT_PROCEDURE("uniform1fv", 2, 0, 0, glue_glUniform1fv);
    EXPORT_PROCEDURE("uniform2fv", 2, 0, 0, glue_glUniform2fv);
    EXPORT_PROCEDURE("uniform3fv", 2, 0, 0, glue_glUniform3fv);
    EXPORT_PROCEDURE("uniform4fv", 2, 0, 0, glue_glUniform4fv);
    EXPORT_PROCEDURE("uniform1uiv", 2, 0, 0, glue_glUniform1uiv);
    EXPORT_PROCEDURE("uniform2uiv", 2, 0, 0, glue_glUniform2uiv);
    EXPORT_PROCEDURE("uniform3uiv", 2, 0, 0, glue_glUniform3uiv);
    EXPORT_PROCEDURE("uniform4uiv", 2, 0, 0, glue_glUniform4uiv);
    EXPORT_PROCEDURE("uniform1iv", 2, 0, 0, glue_glUniform1iv);
    EXPORT_PROCEDURE("uniform2iv", 2, 0, 0, glue_glUniform2iv);
    EXPORT_PROCEDURE("uniform3iv", 2, 0, 0, glue_glUniform3iv);
    EXPORT_PROCEDURE("uniform4iv", 2, 0, 0, glue_glUniform4iv);

    EXPORT_PROCEDURE("uniform-matrix3", 3, 0, 0, glue_glUniformMatrix3);
    EXPORT_PROCEDURE("uniform-matrix4", 3, 0, 0, glue_glUniformMatrix4);

    EXPORT_PROCEDURE("uniform-matrix3v", 3, 0, 0, glue_glUniformMatrix3v);
    EXPORT_PROCEDURE("uniform-matrix4v", 3, 0, 0, glue_glUniformMatrix4v);

    EXPORT_PROCEDURE("draw-arrays", 3, 0, 0, glue_glDrawArrays);
}


void clear_gl_data () {
    for (int i = 0; i < MAX_SHADERS; i++) {
        if (MODEL->shaders[i]) {
            glDeleteShader(i);
            MODEL->shaders[i] = false;
        }
    }

    for (int i = 0; i < MAX_PROGRAMS; i++) {
        if (MODEL->programs[i]) {
            glDeleteProgram(i);
            MODEL->programs[i] = false;
        }
    }

    for (int i = 0; i < MAX_VBOS; i++) {
        if (MODEL->vbos[i]) {
            glDeleteBuffers(1, &i);
            MODEL->vbos[i] = false;
        }
    }

    for (int i = 0; i < MAX_VAOS; i++) {
        if (MODEL->vaos[i]) {
            glDeleteVertexArrays(1, &i);
            MODEL->vaos[i] = false;
        }
    }

    glClearColor(0.0, 0.0, 0.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}
