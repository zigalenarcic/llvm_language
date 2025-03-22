#ifndef _PARSER_H_
#define _PARSER_H_

typedef char i8;
typedef unsigned char u8;
typedef short i16;
typedef unsigned short u16;
typedef int i32;
typedef unsigned int u32;
typedef long int i64;
typedef unsigned long int u64;

enum
{
  TOK_EOF = 0,
  TOK_STRING,
  TOK_INTEGER,
  TOK_FLOAT,
  TOK_IDENTIFIER,

  TOK_LEFT_PAREN,
  TOK_RIGHT_PAREN,
  TOK_LEFT_BRACKET,
  TOK_RIGHT_BRACKET,
  TOK_LEFT_BRACE,
  TOK_RIGHT_BRACE,

  TOK_PLUS,
  TOK_MINUS,
  TOK_TIMES,
  TOK_DIV,

  TOK_EQUALS,
  TOK_COMMA,
  TOK_COLON,
  TOK_SEMICOLON,

  TOK_DECLARE,
  TOK_IF,
  TOK_FOR,
  TOK_ERROR,
};

typedef struct Token_
{
  int type;
  char string[128];
  u64 integer;
  double floatval;

  int pos;
} Token;

typedef struct TokenizerState_
{

  const char *begin;
  const char *end;
  const char *it;

  int state;
  char buffer[10 * 1024];
  int buffer_pos;

  Token t1;
  int paren_count;
} TokenizerState;

void InitTokenizer(TokenizerState *state, const char *code);
Token GetToken(TokenizerState *state);

enum {
  NODE_INTEGER,
  NODE_FLOAT,
  NODE_STRING,
  NODE_FUNCALL,
  NODE_IDENTIFIER,

  NODE_ADD,
  NODE_MULTIPLY,
  NODE_SUBTRACT,
  NODE_DIVIDE,
  NODE_NEGATE,

  NODE_DECLARE,
  NODE_FUNCTION_DEFINITION,
  NODE_ASSIGN,
};

typedef enum {
  TYPE_UNKNOWN = 0,
  TYPE_I64,
  TYPE_U64,
  TYPE_F64,
  TYPE_F32,
  TYPE_U8,
  TYPE_I8,
  TYPE_U16,
  TYPE_I16,
  TYPE_U32,
  TYPE_I32,
  TYPE_PTR,
  TYPE_VOID,
} ETypes;

typedef struct AstNode {
  int type;
  Token token;
  struct AstNode *args;
  struct AstNode *args2; /* second part of data - function body etc. */

  int return_type;
  /* function data */
  Token function_name;

  struct AstNode *next;
} AstNode;

void InitParser(TokenizerState *state);
void CodeError(const char *code_begin, const char *code_end, int pos, const char *fmt, ...);
const char *NodeToString(int type);
AstNode *ParseToplevel(TokenizerState *state);
void PrintAstNode(AstNode *node);
void FreeAstNode(AstNode *node);

#endif // _PARSER_H_

