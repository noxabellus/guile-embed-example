#include <stdio.h>
#include <stdbool.h>
#include <libguile.h>

typedef union {
  struct {
    int x;
    int y;
  } move;
} CommandData;

typedef enum {
  CMD_MOVE,
  CMD_PEN_UP,
  CMD_PEN_DOWN,
} CommandType;

typedef struct {
  CommandType type;
  CommandData data;
} Command;

Command COMMANDS[10000];
int COMMANDS_LENGTH = 0;

void cmd_push (Command cmd) {
  COMMANDS[COMMANDS_LENGTH] = cmd;
  COMMANDS_LENGTH++;
}

void cmd_move (int x, int y) {
  cmd_push((Command) {
    .type = CMD_MOVE,
    .data.move.x = x,
    .data.move.y = y
  });
}

void cmd_pen_up () {
  cmd_push((Command) {.type = CMD_PEN_UP});
}

void cmd_pen_down () {
  cmd_push((Command) {.type = CMD_PEN_DOWN});
}

SCM cmd_move_scm (SCM x, SCM y) {
  cmd_move(scm_to_int(x), scm_to_int(y));
  return SCM_UNSPECIFIED;
}

SCM cmd_pen_up_scm () {
  cmd_pen_up();
  return SCM_UNSPECIFIED;
}

SCM cmd_pen_down_scm () {
  cmd_pen_down();
  return SCM_UNSPECIFIED;
}

static
void* register_functions (void* data) {
  scm_c_define_gsubr("move", 2, 0, 0, cmd_move_scm);
  scm_c_define_gsubr("pen-up", 0, 0, 0, cmd_pen_up_scm);
  scm_c_define_gsubr("pen-down", 0, 0, 0, cmd_pen_down_scm);
  return NULL;
}


SCM run_scm (void*) {
  scm_c_primitive_load("scripts/turtle.scm");

  printf("loaded turtle.scm\n");

  SCM var = scm_c_lookup("run-turtle");
  SCM ref = scm_variable_ref(var);

  scm_call_1(ref, scm_from_utf8_string("(input from C here)"));

  for (int i = 0; i < COMMANDS_LENGTH; i++) {
    Command cmd = COMMANDS[i];
    switch (cmd.type) {
      case CMD_MOVE:
        printf("move %d %d\n", cmd.data.move.x, cmd.data.move.y);
        break;
      case CMD_PEN_UP:
        printf("pen up\n");
        break;
      case CMD_PEN_DOWN:
        printf("pen down\n");
        break;
    }
  }

  return SCM_UNSPECIFIED;
}

SCM handle_exn (void*, SCM key, SCM args) {
  SCM str = scm_symbol_to_string(key);
  SCM str2 = scm_object_to_string(args, SCM_UNDEFINED);

  char* key_s = scm_to_utf8_string(str);
  char* args_s = scm_to_utf8_string(str2);

  printf("'%s exception in script: %s\n", key_s, args_s);

  exit(1);
}

int main (int argc, char** argv) {
  scm_with_guile(&register_functions, NULL);


  scm_c_catch(scm_from_bool(true), run_scm, NULL, NULL, NULL, handle_exn, NULL);
  
  return 0;
}