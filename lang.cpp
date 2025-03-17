#include <cstdio>
#include <cstdarg>
#include <cstring>
#include <cmath>
#include <sys/mman.h>

#include <vector>

typedef char i8;
typedef unsigned char u8;
typedef short i16;
typedef unsigned short u16;
typedef int i32;
typedef unsigned int u32;
typedef long int i64;
typedef unsigned long int u64;

///////////////////////////////////////////////////////////////////////////////
// 1 PARSER
///////////////////////////////////////////////////////////////////////////////

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

void InitTokenizer(TokenizerState *state, const char *code)
{
  state->begin = code;
  state->end = code + strlen(code);
  state->it = code;

  state->buffer_pos = 0;
  state->state = 0;
}

enum {
  STATE_COMMENT,
  STATE_IDENTIFIER,
  STATE_STRING,
  STATE_INTEGER,
  STATE_FLOAT,
};

int ReadWhitespace(const char **it, const char *end)
{
  while (*it != end && **it != '\0' &&
      (**it == ' ' || **it == '\t' || **it == '\r' || **it == '\n'))
  {
    (*it)++;
  }

  return *it == end || **it == '\0' ? -1 : 0;
}

void CopyChar(TokenizerState *state)
{
  state->buffer[state->buffer_pos++] = *state->it++;
}

void CopyCharAs(TokenizerState *state, char c)
{
  state->buffer[state->buffer_pos++] = c;
  state->it++;
}

bool IsIdentifierChar(char c)
{
  if ((c >= '0' && c <= '9')

      || (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c == '_'))
    return true;

  return false;
}

bool IsNumeric(char c)
{
  if (c >= '0' && c <= '9')
    return true;

  return false;
}

bool IsNumericFloat(char c)
{
  if (c >= '0' && c <= '9')
    return true;
  if ((c == '.') || (c == 'e'))
    return true;

  return false;
}

struct
{
  const char *str;
  int type;
} specializations[] =
{
  { "declare", TOK_DECLARE },
};

bool SpecalizeIdentifier(Token *t)
{
  for (int i = 0; i < sizeof(specializations)/sizeof(specializations[0]); i++)
  {
    if (strcmp(t->string, specializations[i].str) == 0)
    {
      t->type = specializations[i].type;
      return true;
    }
  }

  return false;
}

Token GetToken(TokenizerState *state)
{
  Token t;
  t.type = TOK_EOF;
  char c = '\0';

  while (true)
  {
    if (ReadWhitespace(&state->it, state->end) != 0)
    {
      t.pos = (int)(state->it - state->begin);
      t.type = TOK_EOF;
      return t;
    }

    t.pos = (int)(state->it - state->begin);
    c = *state->it;

    //printf("pos %ld, next char %c\n", state->it - state->begin, c);

    switch (c)
    {
      case '"': state->state = STATE_STRING; break;
      case '=': t.type = TOK_EQUALS; state->it++; return t;
      case ',': t.type = TOK_COMMA; state->it++; return t;
      case ':': t.type = TOK_COLON; state->it++; return t;
      case ';': t.type = TOK_SEMICOLON; state->it++; return t;
      case '(': t.type = TOK_LEFT_PAREN; state->it++; return t;
      case ')': t.type = TOK_RIGHT_PAREN; state->it++; return t;
      case '[': t.type = TOK_LEFT_BRACKET; state->it++; return t;
      case ']': t.type = TOK_RIGHT_BRACKET; state->it++; return t;
      case '{': t.type = TOK_LEFT_BRACE; state->it++; return t;
      case '}': t.type = TOK_RIGHT_BRACE; state->it++; return t;
      case '+': t.type = TOK_PLUS; state->it++; return t;
      case '-': t.type = TOK_MINUS; state->it++; return t;
      case '*': t.type = TOK_TIMES; state->it++; return t;
      case '/': if ((state->it + 1 < state->end) && (state->it[1] == '/'))
                {
                  state->state = STATE_COMMENT;
                  state->it += 2;
                  break;
                }
                else
                {
                  t.type = TOK_DIV; state->it++; return t;
                }
      default:
                if (c >= '0' && c <= '9')
                {
                  state->state = STATE_INTEGER;
                }
                else
                {
                  state->state = STATE_IDENTIFIER;
                }
                break;
    }

    state->buffer_pos = 0;

rescan:
    switch (state->state)
    {
      case STATE_COMMENT:
        while (state->it != state->end && *state->it != '\r' && *state->it != '\n')
          state->it++;
        break;
      case STATE_IDENTIFIER:
        while (state->it != state->end && IsIdentifierChar(*state->it))
        {
          CopyChar(state);
        }
        state->buffer[state->buffer_pos] = '\0';
        t.type = TOK_IDENTIFIER;
        strcpy(t.string, state->buffer);
        SpecalizeIdentifier(&t);
        return t;
      case STATE_STRING:
        state->it++;
        while (state->it != state->end)
        {
          char c = *state->it;

          if (c == '\"')
          {
            state->it++;
            break;
          }
          else if (c == '\\')
          {
            state->it++;
            if (state->it == state->end)
            {
              t.type = TOK_ERROR;
              strcpy(t.string, "Error while parsing string\n");
              t.pos = (int)(state->it - 1 - state->begin);
              return t;
            }

            switch (*state->it)
            {
              case '"':
              case '\\':
                CopyChar(state);
                break;
              case 'n':
                CopyCharAs(state, '\n');
                break;
              case 'r':
                CopyCharAs(state, '\r');
                break;
              case 't':
                CopyCharAs(state, '\t');
                break;
              case 'x':
                break;
            }
          }
          else
          {
            CopyChar(state);
          }
        }
        state->buffer[state->buffer_pos] = '\0';
        t.type = TOK_STRING;
        strcpy(t.string, state->buffer);
        return t;
        break;
      case STATE_INTEGER:
        while (state->it != state->end && IsNumeric(*state->it))
        {
          CopyChar(state);
        }
        if (state->it != state->end && *state->it == '.')
        {
          state->state = STATE_FLOAT;
          goto rescan;
        }
        state->buffer[state->buffer_pos] = '\0';
        t.type = TOK_INTEGER;
        t.integer = strtoul(state->buffer, NULL, 10);
        return t;
      case STATE_FLOAT:
        while (state->it != state->end && IsNumericFloat(*state->it))
        {
          CopyChar(state);
        }
        state->buffer[state->buffer_pos] = '\0';
        t.type = TOK_FLOAT;
        t.floatval = strtod(state->buffer, NULL);
        return t;
    }
  }
}

Token PeekToken(TokenizerState *state)
{
  return state->t1;
}

Token PopToken(TokenizerState *state)
{
  Token ret = state->t1;
  state->t1 = GetToken(state);
  return ret;
}

void InitParser(TokenizerState *state)
{
  state->t1 = GetToken(state);
  state->paren_count = 0;
}

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

AstNode *MakeAstNode(int type, Token t)
{
  AstNode *node = new AstNode;
  memset(node, 0, sizeof(AstNode));
  node->type = type;
  node->token = t;

  return node;
}

AstNode *AddAstNode(AstNode **first, AstNode **last, AstNode *node)
{
  if (*first == NULL)
  {
    *first = *last = node;
  }
  else
  {
    (*last)->next = node;
    *last = node;
  }

  return node;
}

void CodeError(const char *code_begin, const char *code_end, int pos, const char *fmt, ...)
{
  char tmp[1024];
  va_list list;
  va_start(list, fmt);
  vsnprintf(tmp, sizeof(tmp), fmt, list);
  va_end(list);

  int line = 1;
  const char *line_start = code_begin;

  const char *it = code_begin;

  while (it != code_end && (it - code_begin) < pos)
  {
    if (*it == '\n')
    {
      line++;
      line_start = it + 1;
    }
    it++;
  }

  int column = (int)(it - line_start);

  const char *line_end = line_start;
  while (it != code_end)
  {
    if ((*it == '\n') || (*it == '\r'))
    {
      line_end = it;
      break;
    }
    it++;
  }

  fprintf(stderr, "Line %d, column %d: %s", line, column + 1, tmp);
  fprintf(stderr, "%.*s\n", (int)(line_end - line_start), line_start);
  for (int i = 0; i < column; i++)
    fprintf(stderr, " ");
  fprintf(stderr, "^\n");
}

int GetPrecedence(int type)
{
  switch (type)
  {
    case TOK_PLUS:
    case TOK_MINUS:
      return 1;
    case TOK_TIMES:
    case TOK_DIV:
      return 10;
    default:
      return -1;
  }
}

void ParseList(AstNode **node, TokenizerState *state, int separator, int end_token);
void ParseBody(AstNode **node, TokenizerState *state);

AstNode *ParseExpression(TokenizerState *state, int end_token, int end_token2, int current_precedence)
{
  AstNode *expr = NULL;

  while (true)
  {
    Token t1 = PeekToken(state);

    if (t1.type == end_token)
      return expr;

    if (t1.type == end_token2)
      return expr;

    if (t1.type == TOK_EOF)
      return expr;

    if (GetPrecedence(t1.type) >= 0 && current_precedence > GetPrecedence(t1.type))
    {
      if (t1.type != TOK_MINUS)
        return expr;
    }

    PopToken(state);

    // printf("token %d\n", t1.type);

    switch (t1.type)
    {
      case TOK_EQUALS:
        {
          if (!expr)
          {
            CodeError(state->begin, state->end, t1.pos, "Error: Unexpected token\n");
            return NULL;
          }

          if (expr->type == NODE_IDENTIFIER)
          {
            AstNode *ret = MakeAstNode(NODE_ASSIGN, t1);
            ret->args = expr;
            expr = ret;
            expr->args2 = ParseExpression(state, TOK_SEMICOLON, -1, 0);
          }
          else if (expr->type == NODE_FUNCALL)
          {
            for (AstNode *node = expr->args; node; node = node->next)
            {
              if (node->type != NODE_IDENTIFIER)
              {
                CodeError(state->begin, state->end, node->token.pos, "Error: In function definition argument must be a variable\n");
                return NULL;
              }
            }

            expr->type = NODE_FUNCTION_DEFINITION;
            expr->function_name = expr->token;

            Token t3 = PeekToken(state);

            if (t3.type == TOK_LEFT_BRACE)
            {
              PopToken(state);
              /* function definition */
              ParseBody(&expr->args2, state);
            }
            else
            {
              expr->args2 = ParseExpression(state, TOK_SEMICOLON, -1, 0);
            }
            return expr;
          }

        }
        break;
      case TOK_MINUS:
        if (!expr) /* allow single argument negation */
        {
          AstNode *node1 = ParseExpression(state, end_token, end_token2, 15);

          if (!node1)
          {
            CodeError(state->begin, state->end, t1.pos, "Error: Missing argument\n");
            return NULL;
          }
          expr = MakeAstNode(NODE_NEGATE, t1);
          expr->args = node1;
          break;
        }
      case TOK_PLUS:
      case TOK_TIMES:
      case TOK_DIV:
        {
          if (!expr)
          {
            CodeError(state->begin, state->end, t1.pos, "Error: Unexpected token\n");
            return NULL;
          }
          AstNode *node1 = expr;
          AstNode *node2 = ParseExpression(state, end_token, end_token2, GetPrecedence(t1.type));

          if (!node2)
          {
            CodeError(state->begin, state->end, t1.pos, "Error: Failed to parse second operand\n");
            return NULL;
          }
          expr = MakeAstNode(t1.type == TOK_PLUS ? NODE_ADD :
              t1.type == TOK_MINUS ? NODE_SUBTRACT : t1.type == TOK_TIMES ? NODE_MULTIPLY : NODE_DIVIDE, t1);
          expr->args = node1;
          node1->next = node2;
        }
        break;
      case TOK_LEFT_PAREN:
        state->paren_count++;
        expr = ParseExpression(state, TOK_RIGHT_PAREN, -1, 0);
        if (!expr)
        {
          CodeError(state->begin, state->end, t1.pos, "Error: Empty parenthesis\n");
          return NULL;
        }
        else
        {
          PopToken(state);
          state->paren_count--;
        }
        break;
      case TOK_INTEGER:
        {
          if (expr)
          {
            CodeError(state->begin, state->end, t1.pos, "Error: Unexpected token\n");
            return NULL;
          }

          expr = MakeAstNode(NODE_INTEGER, t1);
        }
        break;
      case TOK_FLOAT:
        {
          if (expr)
          {
            CodeError(state->begin, state->end, t1.pos, "Error: Unexpected token\n");
            return NULL;
          }

          expr = MakeAstNode(NODE_FLOAT, t1);
        }
        break;
      case TOK_STRING:
        {
          if (expr)
          {
            CodeError(state->begin, state->end, t1.pos, "Error: Unexpected token\n");
            return NULL;
          }

          expr = MakeAstNode(NODE_STRING, t1);
        }
        break;
      case TOK_DECLARE:
        {
          if (expr)
          {
            CodeError(state->begin, state->end, t1.pos, "Error: Unexpected token\n");
            return NULL;
          }

          Token t2 = PeekToken(state);
          if (t2.type == TOK_LEFT_PAREN)
          {
            PopToken(state);
            expr = MakeAstNode(NODE_DECLARE, t1);

            ParseList(&expr->args, state, TOK_COMMA, TOK_RIGHT_PAREN);
          }
          else
          {
            return NULL;
          }
        }
        break;
      case TOK_IDENTIFIER:
        {
          Token t2 = PeekToken(state);
          if (t2.type == TOK_LEFT_PAREN)
          {
            PopToken(state);
            expr = MakeAstNode(NODE_FUNCALL, t1);

            ParseList(&expr->args, state, TOK_COMMA, TOK_RIGHT_PAREN);
          }
          else
          {
            if (expr)
            {
              CodeError(state->begin, state->end, t1.pos, "Error: Unexpected token\n");
              return NULL;
            }

            expr = MakeAstNode(NODE_IDENTIFIER, t1);
          }
        }
        break;
      default:
        CodeError(state->begin, state->end, t1.pos, "Error: Unexpected token\n");
        return NULL;
        break;
    }
  }

  return expr;
}

void ParseList(AstNode **node, TokenizerState *state, int separator, int end_token)
{
  AstNode *last = NULL;

  while (true)
  {
    Token t1 = PeekToken(state);

    if (t1.type == end_token)
    {
      PopToken(state);
      return;
    }
    else if (t1.type == TOK_COMMA)
    {
      PopToken(state);
    }
    else if (t1.type == TOK_EOF)
    {
      CodeError(state->begin, state->end, t1.pos, "Error: Missing token\n");
      return;
    }
    else
    {
      AstNode *expr = ParseExpression(state, separator, end_token, 0);
      if (expr)
      {
        AddAstNode(node, &last, expr);
      }
    }
  }
}

void ParseBody(AstNode **node, TokenizerState *state)
{
  AstNode *last = NULL;

  while (true)
  {
    Token t1 = PeekToken(state);

    if (t1.type == TOK_RIGHT_BRACE)
    {
      PopToken(state);
      return;
    }
    else if (t1.type == TOK_SEMICOLON)
    {
      PopToken(state);
    }
    else if (t1.type == TOK_EOF)
    {
      CodeError(state->begin, state->end, t1.pos, "Error: Missing token\n");
      return;
    }
    else
    {
      AstNode *expr = ParseExpression(state, TOK_SEMICOLON, TOK_RIGHT_BRACE, 0);
      if (expr)
      {
        AddAstNode(node, &last, expr);
      }
    }
  }
}

AstNode *ParseToplevel(TokenizerState *state)
{
  AstNode *first = NULL;
  AstNode *last = NULL;

  while (true)
  {
    AstNode *expr = ParseExpression(state, TOK_SEMICOLON, -1, 0);
    if (expr)
    {
      AddAstNode(&first, &last, expr);
    }

    Token t1 = PopToken(state);

    if (t1.type == TOK_EOF)
      return first;
  }

  return first;
}

const char *NodeToString(int type)
{
  switch (type)
  {
    case NODE_FUNCALL: return "FUNCALL";
    case NODE_INTEGER: return "INTEGER";
    case NODE_FLOAT: return "FLOAT";
    case NODE_STRING: return "STRING";
    case NODE_ADD: return "ADD";
    case NODE_MULTIPLY: return "MULTIPLY";
    case NODE_SUBTRACT: return "SUBTRACT";
    case NODE_DIVIDE: return "DIVIDE";
    case NODE_NEGATE: return "NEGATE";
    case NODE_IDENTIFIER: return "IDENTIFIER";
    case NODE_DECLARE: return "DECLARE";
    case NODE_FUNCTION_DEFINITION: return "FUNCTION_DEFINITION";
    case NODE_ASSIGN: return "ASSIGN";
  }
  return "?";
}

void PrintAstNode1(AstNode *node, int level)
{
  while (node != NULL)
  {
    for (int i = 0; i < level; i++)
      printf("  ");
    printf("Node: %d (%s)", node->type, NodeToString(node->type));
    switch (node->type)
    {
      case NODE_INTEGER:
        printf(" value %lu\n", node->token.integer);
        break;
      case NODE_FLOAT:
        printf(" value %f\n", node->token.floatval);
        break;
      case NODE_STRING:
        printf(" value \"%s\"\n", node->token.string);
        break;
      case NODE_FUNCALL:
        printf(" %s\n", node->token.string);
        if (node->args)
          PrintAstNode1(node->args, level + 1);
        break;
      case NODE_IDENTIFIER:
        printf(" value %s\n", node->token.string);
        break;
      case NODE_ASSIGN:
        printf(" var:\n");
        if (node->args)
          PrintAstNode1(node->args, level + 1);
        printf(" value:\n");
        if (node->args2)
          PrintAstNode1(node->args2, level + 1);
        break;
      case NODE_DECLARE:
        printf(" declaration name \"%s\" args:\n", node->function_name.string);
        if (node->args)
          PrintAstNode1(node->args, level + 1);
        break;
      case NODE_FUNCTION_DEFINITION:
        printf(" function name \"%s\" args:\n", node->function_name.string);
        if (node->args)
          PrintAstNode1(node->args, level + 1);
        for (int i = 0; i < level; i++)
          printf("  ");
        printf("    function body:\n");
        if (node->args2)
          PrintAstNode1(node->args2, level + 1);
        break;
      default:
        printf("\n");
        if (node->args)
          PrintAstNode1(node->args, level + 1);
        break;
    }

    node = node->next;
  }
}

void PrintAstNode(AstNode *node)
{
  printf("AstNode %p:\n", node);
  if (node)
  {
    PrintAstNode1(node, 1);
  }
}

void FreeAstNode(AstNode *node)
{
  while (node != NULL)
  {
    if (node->args)
      FreeAstNode(node->args);

    if (node->args2)
      FreeAstNode(node->args2);

    AstNode *next = node->next;

    delete node;

    node = next;
  }
}

///////////////////////////////////////////////////////////////////////////////
// 2 COMPILER
///////////////////////////////////////////////////////////////////////////////

/* building IR */
#include "llvm/ADT/APFloat.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
/* JIT */
#include "llvm/Support/InitLLVM.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"

typedef struct {
  int type;
  const char *name;
  const char *name_lowercase;
  int bits;
  bool floating_point;
  bool sign;
  int alt_type;
} TypeDescription;

TypeDescription type_descriptions[] = {
  { TYPE_UNKNOWN, "UNKNOWN", "unknown", 0, false, false, TYPE_UNKNOWN},
  { TYPE_VOID, "VOID", "void", 0, false, false, TYPE_VOID},
  { TYPE_I64, "I64", "i64", 64, false, true, TYPE_U64},
  { TYPE_U64, "U64", "u64", 64, false, false, TYPE_I64},
  { TYPE_F64, "F64", "f64", 64, true, true, TYPE_F64},
  { TYPE_F32, "F32", "f32", 32, true, true, TYPE_F64},
  { TYPE_U8, "U8", "u8", 8, false, false, TYPE_I8},
  { TYPE_I8, "I8", "i8", 8, false, true, TYPE_U8},
  { TYPE_U16, "U16", "u16", 16, false, false, TYPE_I16},
  { TYPE_I16, "I16", "i16", 16, false, true, TYPE_U16},
  { TYPE_U32, "U32", "u32", 32, false, false, TYPE_I32},
  { TYPE_I32, "I32", "i32", 32, false, true, TYPE_U32},
  { TYPE_PTR, "PTR", "ptr", 64, false, false, TYPE_PTR},
};

typedef struct VariableDefinition {
  char name[128];
  int type;
  llvm::Value *value;
} VariableDefinition;

typedef struct GlobalVariableDef {
  char name[128];
  int type;
  bool constant;
} GlobalVariableDef;

typedef struct Scope {
  std::vector<VariableDefinition> variables;
} Scope;

typedef struct FunctionInfo {
  char name[128];
  int num_arguments;

  bool has_types;
  int return_type;
  std::vector<int> arg_types;

  bool has_definition;
  AstNode *definition;

  int patch;
} FunctionInfo;

typedef struct CompileEnvironment {
  std::unique_ptr<llvm::LLVMContext> context;
  llvm::IRBuilder<> *builder;
  std::unique_ptr<llvm::Module> mod;

  char module_name[64];
  char toplevel_function_name[64];

  std::vector<FunctionInfo *> functions;
  std::vector<GlobalVariableDef> global_variables;

  /* data for current compilation */
  std::vector<FunctionInfo *> functions_to_declare;
  std::vector<std::pair<char *, char *>> patches;

  // source code (for printing errors)
  const char *code_begin;
  const char *code_end;
} CompileEnvironment;

struct {
  const char *name;
  int type;
} specialization_type[] = {
  { "i64", TYPE_I64},
  { "u64", TYPE_U64},
  { "f64", TYPE_F64},
  { "f32", TYPE_F32},
  { "u8", TYPE_U8},
  { "i8", TYPE_I8},
  { "u16", TYPE_U16},
  { "i16", TYPE_I16},
  { "u32", TYPE_U32},
  { "i32", TYPE_I32},
};

TypeDescription *FindTypeDescription(int type)
{
  for (int i = 0; i < sizeof(type_descriptions) / sizeof(type_descriptions[0]); i++)
  {
    //if (strcmp(type_descriptions[i].name, arg->token.string) == 0)
    if (type_descriptions[i].type == type)
      return &type_descriptions[i];
  }

  return NULL;
}

int FindType(int bits, bool sign, bool floating_point)
{
  for (int i = 0; i < sizeof(type_descriptions) / sizeof(type_descriptions[0]); i++)
  {
    if ((type_descriptions[i].bits == bits) &&
        (type_descriptions[i].sign == sign) &&
        (type_descriptions[i].floating_point == floating_point))
      return type_descriptions[i].type;
  }

  return TYPE_UNKNOWN;
}

int IdentifierToType(AstNode *arg)
{
  if (arg->type == NODE_IDENTIFIER)
  {
    for (int i = 0; i < sizeof(specialization_type) / sizeof(specialization_type[0]); i++)
    {
      if (strcmp(specialization_type[i].name, arg->token.string) == 0)
        return specialization_type[i].type;
    }
  }
  return TYPE_UNKNOWN;
}

llvm::Type *LlvmType(int type, llvm::LLVMContext *context)
{
  switch (type)
  {
    case TYPE_UNKNOWN:
      return NULL;
      break;
    case TYPE_VOID:
      return llvm::Type::getVoidTy(*context);
      break;
    case TYPE_I64:
    case TYPE_U64:
    default:
      return llvm::Type::getInt64Ty(*context);
      break;
    case TYPE_F64:
      return llvm::Type::getDoubleTy(*context);
      break;
    case TYPE_F32:
      return llvm::Type::getFloatTy(*context);
      break;
    case TYPE_I32:
    case TYPE_U32:
      return llvm::Type::getInt32Ty(*context);
      break;
    case TYPE_I16:
    case TYPE_U16:
      return llvm::Type::getInt16Ty(*context);
      break;
    case TYPE_I8:
    case TYPE_U8:
      return llvm::Type::getInt8Ty(*context);
      break;
  }
}

const char *TypeToString(int type)
{
  TypeDescription *td = FindTypeDescription(type);
  if (td)
    return td->name;
  else
    return "UNKNOWN";
}

int UprateTypes(int typea, int typeb)
{
  TypeDescription *t1 = FindTypeDescription(typea);
  TypeDescription *t2 = FindTypeDescription(typeb);

  int max_bits_type = t1->bits;
  if (t2->bits > t1->bits)
    max_bits_type = t2->bits;

  int ret = FindType(max_bits_type, t1->sign || t2->sign, t1->floating_point || t2->floating_point);

  printf("Uprate %s, %s -> %s\n", TypeToString(typea), TypeToString(typeb), TypeToString(ret));
  return ret;
}

VariableDefinition *FindLocalVariable(Scope *scope, const char *name)
{
  if (!scope)
  {
    return NULL;
  }

  for (int i = scope->variables.size() - 1; i >= 0; i--)
  {
    if (strcmp(scope->variables[i].name, name) == 0)
    {
      return &scope->variables[i];
    }
  }

  return NULL;
}

llvm::Function *AddFunctionDeclarationToModule(const char *name, int return_type, std::vector<int> &arg_types, llvm::LLVMContext *context, llvm::Module *mod)
{
  printf("Adding function declaration to module: ");
  {
    printf("Function: \"%s\" %s(", name, TypeToString(return_type));
    for (int i = 0; i < arg_types.size(); i++)
    {
      printf(i == 0 ? "%s" : " %s", TypeToString(arg_types[i]));
    }
    printf(")\n");
  }

  std::vector<llvm::Type *> types;
  for (auto &a : arg_types)
  {
    types.push_back(LlvmType(a, context));
  }

  llvm::FunctionType *ft = llvm::FunctionType::get(LlvmType(return_type, context), types, false);

  llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, *mod);

  return f;
}

llvm::GlobalVariable *FindGlobalVariable(CompileEnvironment *cenv, const char *name, int type)
{
  llvm::GlobalVariable *var = cenv->mod->getGlobalVariable(name);

  if (var)
    return var;

  for (auto &a : cenv->global_variables)
  {
    if (strcmp(a.name, name) == 0)
    {
      printf("Found global variable %s\n", name);
      if ((type != -1) && (type != a.type))
      {
        printf("Type mismatch %s %s\n", TypeToString(type), TypeToString(a.type));
      }

      auto llvm_type = LlvmType(a.type, cenv->context.get());
      var = new llvm::GlobalVariable(*cenv->mod, llvm_type, false,
          llvm::GlobalValue::ExternalLinkage, nullptr, name);
      break;
    }
  }

  return var;
}

int FindByNamePtr(const void *data, int count, int offset, const char *str)
{
  const char *data8 = (const char *)data;
  for (int i = 0; i < count; i++)
  {
    if (strcmp(data8 + i * sizeof(void *) + offset, str) == 0)
      return i;
  }

  return -1;
}

FunctionInfo *FindFunction(CompileEnvironment *cenv, const char *name)
{
  //int idx = FindByNamePtr(&cenv->functions[0], cenv->functions.size(),
      //offsetof(FunctionInfo, name), name);

  int idx = -1;
  for (int i = 0; i < cenv->functions.size(); i++)
  {
    if (strcmp(cenv->functions[i]->name, name) == 0)
    {
      idx = i;
      break;
    }
  }

  // printf("FindFunction: Function %s - idx %d\n", name, idx);

  if (idx >= 0)
    return cenv->functions[idx];
  else
    return NULL;
}

///////////////////////////////////////////////////////////////////////////////
// 2.2 COMPILE PASSES
///////////////////////////////////////////////////////////////////////////////

/**
 * CollectDeclarations
 *
 * It should traverse the AST tree on the top level and add declarations
 * (explicit and implicit) to structures in CompileEnvironment *cenv.
 *
 * These declaration can be added to the module when the AST tree is being
 * compiled into LLVM IR. If declaration are added earlier than AST tree
 * compilation, the order of function definitions can be arbitrary.
 */
int CollectDeclarations(AstNode *node, Scope *scope, CompileEnvironment *cenv, bool all)
{
  int return_value = 0;
  while (node)
  {
    switch (node->type)
    {
      case NODE_INTEGER:
      case NODE_FLOAT:
      case NODE_STRING:
      case NODE_ADD:
      case NODE_MULTIPLY:
      case NODE_SUBTRACT:
      case NODE_DIVIDE:
      case NODE_IDENTIFIER:
      case NODE_FUNCALL:
        /* nothing to do */
        break;
      case NODE_ASSIGN:
#if 0
        {
          const char *name = node->args->token.string;

          if (!scope)
          {
            int idx = FindByName(&cenv->global_variables[0], sizeof(GlobalVariableDef),
                cenv->global_variables.size(), offsetof(GlobalVariableDef, name), name);

            if (idx < 0)
            {
              /* new global variable */
              printf("Adding global variable: %s\n", name);
              GlobalVariableDef global_var;
              strcpy(global_var.name, name);
              global_var.constant = false;
              cenv->global_variables.push_back(global_var);
            }
          }
        }
#endif
        break;
      case NODE_DECLARE:
        {
          AstNode *arg = node->args;
          int return_type = IdentifierToType(arg);
          if (return_type == TYPE_UNKNOWN)
          {
            CodeError(cenv->code_begin, cenv->code_end, arg->token.pos, "Error: Unknown type\n");
            goto return_error;
          }
          arg = arg->next;
          const char *name = arg->token.string;
          arg = arg->next;

          FunctionInfo *fi = FindFunction(cenv, name);
          if (fi)
          {
            CodeError(cenv->code_begin, cenv->code_end, node->token.pos, "Warning: Redeclaration of the function \"%s\"\n", name);
            goto next_node;
          }
          else
          {
            fi = (FunctionInfo *)calloc(1, sizeof(FunctionInfo));
            fi->has_types = true;

            fi->patch = 0;
            fi->return_type = return_type;
            strcpy(fi->name, name);

            for (; arg; arg = arg->next)
            {
              int type = IdentifierToType(arg);
              if (type == TYPE_UNKNOWN)
              {
                CodeError(cenv->code_begin, cenv->code_end, arg->token.pos, "Error: Unknown type\n");
                goto return_error;
              }
              fi->arg_types.push_back(type);
              fi->num_arguments++;
            }

            printf("Adding function info && declaration: %s\n", fi->name);
            cenv->functions.push_back(fi);
            if (std::find(cenv->functions_to_declare.begin(),
                  cenv->functions_to_declare.end(), fi) == cenv->functions_to_declare.end())
              cenv->functions_to_declare.push_back(fi);
          }
        }
        break;
      case NODE_FUNCTION_DEFINITION:
        {
          FunctionInfo *fi = FindFunction(cenv, node->function_name.string);

          if (fi == nullptr)
          {
            CodeError(cenv->code_begin, cenv->code_end, node->token.pos, "Error: Function doesn't have a declaration\n");
            goto return_error;
#if 0
            FunctionInfo *fi = (FunctionInfo *)calloc(sizeof(FunctionInfo), 1);
            fi->patch = 0;
            fi->num_arguments = 0;
            strcpy(fi->name, node->function_name.string);

            for (AstNode *arg = node->args; arg; arg = arg->next)
              fi->num_arguments++;

            printf("Adding function info: %s\n", fi->name);
            cenv->functions.push_back(fi);
#endif
          }
        }
        break;
      default:
        printf("%s: Unsupported node %s\n", __func__, NodeToString(node->type));
        break;
    }

next_node:
    if (!all)
      return return_value;

    if (!node->next)
      return return_value;

    node = node->next;
  }
  return return_value;

return_error:
  return -1;
}

llvm::Value *ConvertNumberType(CompileEnvironment *cenv, llvm::Value *v, int type_in, int type_out)
{
  if (type_in == type_out)
    return v;

  TypeDescription *td_in = FindTypeDescription(type_in);
  TypeDescription *td_out = FindTypeDescription(type_out);

  if (td_in->floating_point && td_out->floating_point)
    return cenv->builder->CreateFPCast(v, LlvmType(type_out, cenv->context.get()));
  else if (!td_in->floating_point && td_out->floating_point)
    return td_in->sign ? cenv->builder->CreateSIToFP(v, LlvmType(type_out, cenv->context.get())) :
      cenv->builder->CreateUIToFP(v, LlvmType(type_out, cenv->context.get()));
  else if (td_in->floating_point && !td_out->floating_point)
    return td_out->sign ? cenv->builder->CreateFPToSI(v, LlvmType(type_out, cenv->context.get())) :
      cenv->builder->CreateFPToUI(v, LlvmType(type_out, cenv->context.get()));
  else
    return td_in->sign ? cenv->builder->CreateSExtOrTrunc(v, LlvmType(type_out, cenv->context.get())) :
      cenv->builder->CreateZExtOrTrunc(v, LlvmType(type_out, cenv->context.get()));
}

typedef std::pair<llvm::Value *, int> ValueType;

ValueType GenerateIr(AstNode *node, Scope *scope, CompileEnvironment *cenv, bool all, bool emit_ir, bool definitions = false)
{
  ValueType return_value(nullptr, TYPE_UNKNOWN);

  while (node)
  {
    if (!emit_ir && definitions && node->type != NODE_FUNCTION_DEFINITION)
    {
      if (!all)
        return return_value;

      if (!node->next)
        return return_value;

      node = node->next;
    }

    switch (node->type)
    {
      case NODE_INTEGER:
        if (emit_ir)
          return_value.first = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*cenv->context), node->token.integer, false);
        return_value.second = node->return_type = TYPE_I64;
        break;
      case NODE_FLOAT:
        if (emit_ir)
          return_value.first = llvm::ConstantFP::get(*cenv->context, llvm::APFloat(node->token.floatval));
        return_value.second = node->return_type = TYPE_F64;
        break;
      case NODE_STRING:
        if (emit_ir)
          return_value.first = cenv->builder->CreateGlobalString(node->token.string);
        return_value.second = node->return_type = TYPE_PTR;
        break;
      case NODE_ADD:
      case NODE_MULTIPLY:
      case NODE_SUBTRACT:
      case NODE_DIVIDE:
        {
          ValueType a0 = GenerateIr(node->args, scope, cenv, false, emit_ir);
          ValueType b0 = GenerateIr(node->args->next, scope, cenv, false, emit_ir);
          return_value.second = node->return_type = UprateTypes(a0.second, b0.second);
          if (emit_ir)
          {
            llvm::Value *a = a0.first;
            llvm::Value *b = b0.first;

            if (node->return_type == TYPE_F64)
            {
              a = ConvertNumberType(cenv, a, a0.second, TYPE_F64);
              b = ConvertNumberType(cenv, b, b0.second, TYPE_F64);

              if (node->type == NODE_ADD)
                return_value.first = cenv->builder->CreateFAdd(a, b);
              else if (node->type == NODE_MULTIPLY)
                return_value.first = cenv->builder->CreateFMul(a, b);
              else if (node->type == NODE_SUBTRACT)
                return_value.first = cenv->builder->CreateFSub(a, b);
              else if (node->type == NODE_DIVIDE)
                return_value.first = cenv->builder->CreateFDiv(a, b);
            }
            else
            {
              a = ConvertNumberType(cenv, a, a0.second, node->return_type);
              b = ConvertNumberType(cenv, b, b0.second, node->return_type);

              TypeDescription *td_ret = FindTypeDescription(node->return_type);

              if (node->type == NODE_ADD)
                return_value.first = cenv->builder->CreateAdd(a, b);
              else if (node->type == NODE_MULTIPLY)
                return_value.first = cenv->builder->CreateMul(a, b);
              else if (node->type == NODE_SUBTRACT)
                return_value.first = cenv->builder->CreateSub(a, b);
              else if (node->type == NODE_DIVIDE)
                return_value.first = (td_ret && td_ret->sign) ? cenv->builder->CreateSDiv(a, b) :
                  cenv->builder->CreateUDiv(a, b);
            }
          }
        }
        break;
      case NODE_IDENTIFIER:
        {
          const char *name = node->token.string;

          VariableDefinition *local_var = FindLocalVariable(scope, name);
          if (local_var)
          {
            if (emit_ir)
              return_value.first = local_var->value;

            return_value.second = node->return_type = local_var->type;
            break;
          }


          llvm::GlobalVariable *var = FindGlobalVariable(cenv, name, -1);
          if (var)
          {
            if (emit_ir)
              return_value.first = cenv->builder->CreateLoad(var->getValueType(), var, false /* volatile */);

            return_value.second = node->return_type = TYPE_I64; // TODO

            break;
          }

          CodeError(cenv->code_begin, cenv->code_end, node->token.pos, "Error: Variable \"%s\" not found\n", name);
          return std::make_pair<llvm::Value *, int>(nullptr, TYPE_UNKNOWN);
        }
        break;
      case NODE_FUNCALL:
        {
          std::vector<llvm::Value *> args;
          for (AstNode *arg = node->args; arg; arg = arg->next)
          {
            ValueType v = GenerateIr(arg, scope, cenv, false, emit_ir);

            args.push_back(v.first);
          }

          FunctionInfo *fi = FindFunction(cenv, node->token.string);

          llvm::Function *f = nullptr;

          if (fi)
          {
            return_value.second = fi->return_type;
            cenv->functions_to_declare.push_back(fi);
            if (emit_ir)
              f = cenv->mod->getFunction(node->token.string);
          }
          else
          {
            fi = (FunctionInfo *)calloc(1, sizeof(FunctionInfo));
            strcpy(fi->name, node->token.string);
            return_value.second = fi->return_type = TYPE_I64;
            for (AstNode *arg = node->args; arg; arg = arg->next)
            {
              fi->arg_types.push_back(arg->return_type);
            }

            /* ad hoc */
            if (emit_ir)
              f = AddFunctionDeclarationToModule(fi->name, fi->return_type, fi->arg_types, cenv->context.get(), cenv->mod.get());
          }

          if (emit_ir && f)
          {
            if (emit_ir)
              return_value.first = cenv->builder->CreateCall(f, args);
          }
          else
          {
            printf("Error: no function for name \"%s\" found, can't emit function call\n", node->token.string);
            return std::make_pair<llvm::Value *, int>(nullptr, TYPE_UNKNOWN);
          }
        }
        break;
      case NODE_DECLARE:
        /* don't generate instructions */
        break;
      case NODE_FUNCTION_DEFINITION:
        if (emit_ir || definitions)
        {
          const char *name = node->function_name.string;
          FunctionInfo *fi = nullptr;
          FunctionInfo *fi2 = FindFunction(cenv, name);
          if (fi2)
          {
            if (fi2->has_definition)
            {
              /* this definition is a patch of an existing implementation */
              fi = (FunctionInfo *)calloc(1, sizeof(FunctionInfo));
              snprintf(fi->name, sizeof(fi->name), "%s_%d", name, fi2->patch++);
              fi->num_arguments = fi2->num_arguments;
              fi->has_types = fi2->has_types;
              fi->return_type = fi2->return_type;
              fi->arg_types = fi2->arg_types;
              fi->has_definition = fi2->has_definition;

              //fi->definition = node;

              fi->patch = 0;

              cenv->patches.push_back(std::make_pair(strdup(fi2->name), strdup(fi->name)));
            }
            else
            {
              fi = fi2;
            }
          }

          if (!fi)
          {

            CodeError(cenv->code_begin, cenv->code_end, node->token.pos, "Error: Function \"%s\" declaration doesn't exist\n", name);
            return std::make_pair<llvm::Value *, int>(nullptr, TYPE_UNKNOWN);
#if 0
            /* no declaration od definition yet */
            /* provide default types */
            fi = (FunctionInfo *)calloc(1, sizeof(FunctionInfo));
            fi->patch = 0;
            strcpy(fi->name, name);
            fi->return_type = TYPE_I64;

            for (AstNode *arg = node->args; arg; arg = arg->next)
            {
              fi->arg_types.push_back(TYPE_I64);
            }
#endif
          }

          if (!fi->has_definition)
          {
            fi->has_definition = true;
            fi->definition = node;
            printf("Adding definition for function \"%s\"\n", name);
          }

          /* compile */
          /* find existing declaration in this module if present */
          llvm::Function *f = cenv->mod.get()->getFunction(fi->name);
          if (!f)
          {
            /* function is declared in a previous module */
            f = AddFunctionDeclarationToModule(fi->name, fi->return_type, fi->arg_types,
                cenv->context.get(), cenv->mod.get());
          }
          if (!f)
          {
            printf("%d: Something went wrong!\n", __LINE__);
          }
          llvm::BasicBlock *block = llvm::BasicBlock::Create(*cenv->context, "", f);
          auto previous_insert = cenv->builder->saveIP();
          cenv->builder->SetInsertPoint(block);
          cenv->functions.push_back(fi);

          Scope s;
          {
            AstNode *arg = node->args;
              int i = 0;
            for (auto &a : f->args())
            {
              /* get Value types for arguments */
              a.setName(arg->token.string);

              VariableDefinition variable_definition;
              strlcpy(variable_definition.name, arg->token.string, sizeof(variable_definition.name));
              variable_definition.value = &a;
              // variable_definition.type = arg->return_type;
              variable_definition.type = fi->arg_types[i];

              printf("Arg %s type %s\n", variable_definition.name, TypeToString(variable_definition.type));

              s.variables.push_back(variable_definition);

              i++;
              arg = arg->next;
            }
          }

          /* generate IR for the function body */
          if (emit_ir)
          {
          ValueType ret_val = GenerateIr(node->args2, &s, cenv, true, emit_ir);
          cenv->builder->CreateRet(ret_val.first); /* can be null */
          llvm::verifyFunction(*f);
          }
          cenv->builder->restoreIP(previous_insert);
        }
        else
        {
          if (emit_ir)
            return_value.first = nullptr; /* void */
        }
        break;
      case NODE_ASSIGN:
        {
#if 0
          const char *name = node->args->token.string;
          int type = node->args2->return_type;

          if (!scope)
          {
            llvm::GlobalVariable *var = FindGlobalVariable(cenv, name, type);

            if (!var)
            {
              /* add a global variable */
              printf("Adding global variable: %s, type %s\n", name, TypeToString(type));
              GlobalVariableDef global_var;
              strcpy(global_var.name, name);
              global_var.type = type;
              global_var.constant = false;
              cenv->global_variables.push_back(global_var);

              auto llvm_type = LlvmType(type, cenv->context.get());
              var = new llvm::GlobalVariable(*cenv->mod, llvm_type, global_var.constant,
                  llvm::GlobalValue::CommonLinkage, llvm::Constant::getNullValue(llvm_type), name);
            }

            ValueType val = GenerateIr(node->args2, scope, cenv, false, emit_ir);
            cenv->builder->CreateStore(val.first, var, false /* volatile */);
          }
          else
          {
            /* lexical variable */
          }
#endif
        }
        break;
      default:
        printf("%s: Unsupported node %s\n", __func__, NodeToString(node->type));
        break;
    }

    if (!all)
      return return_value;

    if (!node->next)
      return return_value;

    node = node->next;
  }
  return return_value;
}

/**
 * LoadLibrary
 *
 * Should be called from the language itself
 */
extern "C" int LoadLibrary(const char *library_file)
{
  if (llvm::sys::DynamicLibrary::LoadLibraryPermanently(library_file))
  {
    printf("failed to load dynamic library: %s\n", library_file);
    return 1;
  }

  return 0;
}

bool PatchFunctions(llvm::orc::LLJIT *jit, const char *name1, const char *name2)
{
  auto func_addr = jit->lookup(name1);
  auto func_addr2 = jit->lookup(name2);
  if (func_addr && func_addr2)
  {
    u64 addr1 = func_addr->getValue();
    u64 addr2 = func_addr2->getValue();

    // printf("addr1 0x%016lx\n", addr1);
    // printf("addr2 0x%016lx\n", addr2);

    // printf("[addr1] 0x%016lx\n", *(u64 *)addr1);
    // printf("[addr2] 0x%016lx\n", *(u64 *)addr2);

    u64 map_addr = addr1 & ~(4096 -1);
    u8 *code_mmap = (u8 *)mmap((void *)(map_addr), 4096, PROT_WRITE | PROT_READ | PROT_EXEC, MAP_FIXED | MAP_ANONYMOUS | MAP_SHARED, -1, 0);

    // printf("ptr (%p) %p\n", (void *)map_addr, code_mmap);
    u8 *code_function = &code_mmap[addr1 - map_addr];

    /* diff - jump relative address */
    i32 diff = (i64)addr2 - addr1 - 5 /* 5 = jmp instruction length */;
    u64 temp = ((u64)diff << 8) & 0x000000ffffffff00ULL;
    temp |= 0xe9 | 0x909090UL << 40; /* jmp rel32 instruction + nops */

    *(u64 *)code_function = temp;
  }
  else
  {
    printf("Patching: Failed to get function addresses for %s and %s\n", name1, name2);
    return false;
  }

  return true;
}

int main(int argc, char *argv[])
{
  llvm::InitLLVM raii_1(argc, argv); /* helper class for stacktrace, utf-8 argv etc. */

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  auto jit = llvm::orc::LLJITBuilder().create();
  if (!jit)
  {
    fprintf(stderr, "Failed to create a LLJIT object.\n");
    exit(EXIT_FAILURE);
  }

  /* optimization passes */
#if 0
  {
    TheFPM = std::make_unique<FunctionPassManager>();
    TheLAM = std::make_unique<LoopAnalysisManager>();
    TheFAM = std::make_unique<FunctionAnalysisManager>();
    TheCGAM = std::make_unique<CGSCCAnalysisManager>();
    TheMAM = std::make_unique<ModuleAnalysisManager>();
    ThePIC = std::make_unique<PassInstrumentationCallbacks>();
    TheSI = std::make_unique<StandardInstrumentations>(*cenv->context, /*DebugLogging*/ true);

    TheSI->registerCallbacks(*ThePIC, TheMAM.get());

    // Do simple "peephole" optimizations and bit-twiddling optzns.
    TheFPM->addPass(InstCombinePass());
    // Reassociate expressions.
    TheFPM->addPass(ReassociatePass());
    // Eliminate Common SubExpressions.
    TheFPM->addPass(GVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    TheFPM->addPass(SimplifyCFGPass());

    // Register analysis passes used in these transform passes.
    PassBuilder PB;
    PB.registerModuleAnalyses(*TheMAM);
    PB.registerFunctionAnalyses(*TheFAM);
    PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
  }
#endif

  int inputi = -1;
  CompileEnvironment cenv;

  LoadLibrary("libGL.so");
  LoadLibrary("libglfw.so.3");

  i8 *file = NULL;
  if (argc > 1)
  {
    const char *filename = argv[1];
    FILE *f = fopen(filename, "rb");
    if (!f)
      printf("Failed to open file \"%s\" for reading.\n", filename);

    fseek(f, 0, SEEK_END);
    i64 len = ftell(f);
    rewind(f);

    file = (i8 *)calloc(len + 1, 1);
    file[0] = '\0';
    if (fread(file, 1, len, f) != len)
    {
      printf("Failed to read %ld bytes from file \"%s\".\n", len, filename);
    }
    file[len] = '\0';

    fclose(f);
  }

  AstNode *full_ast = nullptr;
  AstNode *full_ast_last = nullptr;

  while (true)
  {
    if (!file)
    {
      printf("> "); fflush(stdin);
    }
    inputi++;

    char line[1024];
    const char *input = NULL;
    if (file)
    {
      input = file;
    }
    else
    {
      input = line;
      if (fgets(line, sizeof(line), stdin) == NULL)
        return 0;
    }

    /* parsing */
    TokenizerState tokenizer_state;
    InitTokenizer(&tokenizer_state, input);

    InitParser(&tokenizer_state);
    AstNode *root_node = ParseToplevel(&tokenizer_state);
    if (!root_node)
      goto next_input;

    PrintAstNode(root_node);

    /* compilation */
    cenv.code_begin = tokenizer_state.begin;
    cenv.code_end = tokenizer_state.end;

    if (CollectDeclarations(root_node, nullptr, &cenv, true) == -1)
    {
      /* error - can't continue */
      FreeAstNode(root_node);
      goto next_input;
    }

    {
      ValueType ret = GenerateIr(root_node, NULL, &cenv, true, false /* emit_ir */, false);
      printf("toplevel ret type %s\n", TypeToString(ret.second));
      int toplevel_return_type = ret.second == TYPE_UNKNOWN ? TYPE_VOID : ret.second;

      /* compile AST to LLVM IR */
      /* create module for this compilation unit */
      snprintf(cenv.module_name, sizeof(cenv.module_name), "m%03i", inputi);
      snprintf(cenv.toplevel_function_name, sizeof(cenv.toplevel_function_name), "m%03i_toplevel", inputi);

      /* init LLVM and create a module */
      cenv.context = std::make_unique<llvm::LLVMContext>(); /* 1 obj per thread */
      cenv.builder = new llvm::IRBuilder<>(*cenv.context);
      cenv.mod = std::make_unique<llvm::Module>(cenv.module_name, *cenv.context);

      /* add function declarations */
      /* after all function declarations are added for this module
       * function definitions can be added in an arbitrary order (LLVM is similar to C file) */
      for (auto &fi : cenv.functions_to_declare)
      {
        AddFunctionDeclarationToModule(fi->name, fi->return_type, fi->arg_types,
            cenv.context.get(), cenv.mod.get());
      }
      cenv.functions_to_declare.clear();

      /* compile function definitions first */
      GenerateIr(root_node, NULL, &cenv, true, true /* emit_ir */, true);

      /* compile toplevel code (add function definitions and toplevel function definition) */
      std::vector<int> empty_vector;
      llvm::Function *f = AddFunctionDeclarationToModule(cenv.toplevel_function_name, toplevel_return_type,
          empty_vector, cenv.context.get(), cenv.mod.get());
      llvm::BasicBlock *block = llvm::BasicBlock::Create(*cenv.context, "", f);
      cenv.builder->SetInsertPoint(block);

      ValueType ret_val = GenerateIr(root_node, NULL, &cenv, true, true /* emit_ir */, false);

      cenv.builder->CreateRet(ret_val.first);
      llvm::verifyFunction(*f);

      /* print module */
      cenv.mod->dump();

      /* this will compile IR code in the module into machine code */
      auto r1 = jit->get()->addIRModule(llvm::orc::ThreadSafeModule(std::move(cenv.mod), std::move(cenv.context)));

      /* patch functions */
      for (auto &a : cenv.patches)
      {
        printf("Patching %s -> %s\n", a.first, a.second);
        PatchFunctions(jit->get(), a.first, a.second);
        free(a.first);
        free(a.second);
      }
      cenv.patches.clear();

      auto func_addr = jit->get()->lookup(cenv.toplevel_function_name);
      printf("Toplevel func addr (%d) 0x%lx\n", func_addr ? 1 : 0, func_addr ? func_addr->getValue() : 0);

      if (func_addr)
      {
        switch (toplevel_return_type)
        {
          case TYPE_VOID:
            {
              ((void (*)())func_addr->getValue())();
              printf("No result (void)\n");
            }
            break;
          case TYPE_U64:
          case TYPE_I64:
            {
              i64 code_result = ((i64 (*)())func_addr->getValue())();
              printf("Result i64 = %ld\n", code_result);
            }
            break;
          case TYPE_U32:
          case TYPE_I32:
            {
              i32 code_result = ((i32 (*)())func_addr->getValue())();
              printf("Result i32 = %d\n", code_result);
            }
            break;
          case TYPE_U16:
          case TYPE_I16:
            {
              i16 code_result = ((i16 (*)())func_addr->getValue())();
              printf("Result i16 = %d\n", code_result);
            }
            break;
          case TYPE_U8:
          case TYPE_I8:
            {
              i8 code_result = ((i8 (*)())func_addr->getValue())();
              printf("Result i8 = %d\n", code_result);
            }
            break;
          case TYPE_F64:
            {
              double code_result = ((double (*)())func_addr->getValue())();
              printf("Result f64 = %f\n", code_result);
            }
            break;
          case TYPE_F32:
            {
              float code_result = ((float (*)())func_addr->getValue())();
              printf("Result f32 = %f\n", code_result);
            }
            break;
        }
      }

      /* append AstNodes in root_node to full_ast */
      if (full_ast)
      {
        full_ast_last->next = root_node;
        while (full_ast_last->next)
          full_ast_last = full_ast_last->next;
      }
      else
      {
        full_ast = full_ast_last = root_node;
        while (full_ast_last->next)
          full_ast_last = full_ast_last->next;
      }
    }


    //printf("Full AST\n");
    //PrintAstNode(full_ast);
next_input:
    if (file)
      break;
  }

  FreeAstNode(full_ast);

  return 0;
}

