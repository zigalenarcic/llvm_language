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
      case '#': state->state = STATE_COMMENT; break;
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
      case '/': t.type = TOK_DIV; state->it++; return t;
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
        state->it++;
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
  TYPE_NONE = 0,
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

  fprintf(stderr, "ERROR: Line %d, column %d: %s", line, column + 1, tmp);
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
            CodeError(state->begin, state->end, t1.pos, "Unexpected token\n");
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
                CodeError(state->begin, state->end, node->token.pos, "In function definition argument must be a variable\n");
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
            CodeError(state->begin, state->end, t1.pos, "Missing argument\n");
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
            CodeError(state->begin, state->end, t1.pos, "Unexpected token\n");
            return NULL;
          }
          AstNode *node1 = expr;
          AstNode *node2 = ParseExpression(state, end_token, end_token2, GetPrecedence(t1.type));

          if (!node2)
          {
            CodeError(state->begin, state->end, t1.pos, "Failed to parse second operand\n");
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
          CodeError(state->begin, state->end, t1.pos, "Empty parenthesis\n");
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
            CodeError(state->begin, state->end, t1.pos, "Unexpected token\n");
            return NULL;
          }

          expr = MakeAstNode(NODE_INTEGER, t1);
        }
        break;
      case TOK_FLOAT:
        {
          if (expr)
          {
            CodeError(state->begin, state->end, t1.pos, "Unexpected token\n");
            return NULL;
          }

          expr = MakeAstNode(NODE_FLOAT, t1);
        }
        break;
      case TOK_STRING:
        {
          if (expr)
          {
            CodeError(state->begin, state->end, t1.pos, "Unexpected token\n");
            return NULL;
          }

          expr = MakeAstNode(NODE_STRING, t1);
        }
        break;
      case TOK_DECLARE:
        {
          if (expr)
          {
            CodeError(state->begin, state->end, t1.pos, "Unexpected token\n");
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
              CodeError(state->begin, state->end, t1.pos, "Unexpected token\n");
              return NULL;
            }

            expr = MakeAstNode(NODE_IDENTIFIER, t1);
          }
        }
        break;
      default:
        CodeError(state->begin, state->end, t1.pos, "Unexpected token\n");
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
      CodeError(state->begin, state->end, t1.pos, "Missing token\n");
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
      CodeError(state->begin, state->end, t1.pos, "Missing token\n");
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
  { TYPE_NONE, "NONE", "none", 0, false, false, TYPE_NONE},
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

const char *TypeToString(int type)
{
  switch (type)
  {
    case TYPE_NONE: return "NONE";
    case TYPE_I64: return "I64";
    case TYPE_U64: return "U64";
    case TYPE_F64: return "F64";
    case TYPE_F32: return "F32";
    case TYPE_U8: return "U8";
    case TYPE_I8: return "I8";
    case TYPE_U16: return "U16";
    case TYPE_I16: return "I16";
    case TYPE_U32: return "U32";
    case TYPE_I32: return "I32";
    case TYPE_VOID: return "VOID";
    default: return "UNKNOWN";
  }
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

  return TYPE_NONE;
}

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

typedef struct VariableDefinition {
  char name[128];
  int type;
  llvm::Value *value;
} VariableDefinition;

typedef struct Scope {
  std::vector<VariableDefinition> variables;
} Scope;

typedef struct FunctionDeclaration {
  char name[128];
  int return_type;
  std::vector<int> arg_types;
  int iteration;
  int num_arguments;
} FunctionDeclaration;

typedef struct GlobalVariableDef {
  char name[128];
  int type;
  bool constant;
} GlobalVariableDef;

typedef struct CompileEnvironment {
  std::unique_ptr<llvm::LLVMContext> context;
  llvm::IRBuilder<> *builder;
  std::unique_ptr<llvm::Module> mod;

  char module_name[64];
  char toplevel_function_name[64];

  std::vector<FunctionDeclaration> function_declarations;
  std::vector<GlobalVariableDef> global_variables;

  std::vector<std::pair<char *, char *>> patches;

  // source code (for printing errors)
  const char *code_begin;
  const char *code_end;
} CompileEnvironment;

VariableDefinition *FindVariable(Scope *scope, const char *name)
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

#if 0
std::unique_ptr<FunctionPassManager> TheFPM;
std::unique_ptr<LoopAnalysisManager> TheLAM;
std::unique_ptr<FunctionAnalysisManager> TheFAM;
std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
std::unique_ptr<ModuleAnalysisManager> TheMAM;
std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
std::unique_ptr<StandardInstrumentations> TheSI;
#endif

llvm::Type *LlvmType(int type, llvm::LLVMContext *context)
{
  switch (type)
  {
    case TYPE_NONE:
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

int LlvmToType(llvm::Type *type)
{
  if (type->isVoidTy())
    return TYPE_VOID;
  if (type->isDoubleTy())
    return TYPE_F64;
  if (type->isFloatTy())
    return TYPE_F32;
  if (type->isIntegerTy(64))
    return TYPE_U64;
  if (type->isIntegerTy(32))
    return TYPE_U32;
  if (type->isIntegerTy(16))
    return TYPE_U16;
  if (type->isIntegerTy(8))
    return TYPE_U8;

  return TYPE_NONE;
}

void PrintFunctionDeclaration(FunctionDeclaration *fun_decl)
{
  printf("Function: \"%s\" %s(", fun_decl->name, TypeToString(fun_decl->return_type));
  for (int i = 0; i < fun_decl->arg_types.size(); i++)
  {
    printf(i == 0 ? "%s" : " %s", TypeToString(fun_decl->arg_types[i]));
  }
  printf(")\n");
}

llvm::Function *AddFunctionDeclaration(FunctionDeclaration *fun_decl, llvm::LLVMContext *context, llvm::Module *mod)
{
  printf("Adding function declaration: ");
  PrintFunctionDeclaration(fun_decl);

  std::vector<llvm::Type *> types;
  for (auto &a : fun_decl->arg_types)
  {
    types.push_back(LlvmType(a, context));
  }
  llvm::FunctionType *ft = llvm::FunctionType::get(LlvmType(fun_decl->return_type, context), types, false);
  //Function *f = mod->getFunction(node->function_name.string);
  llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, fun_decl->name, *mod);

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
  return TYPE_NONE;
}

int FindByName(const void *data, int element_size, int count, int offset, const char *str)
{
  const char *data8 = (const char *)data;
  for (int i = 0; i < count; i++)
  {
    if (strcmp(data8 + i * element_size + offset, str) == 0)
      return i;
  }

  return -1;
}

// COMPILE PASSES

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
        break;
      case NODE_DECLARE:
        {
          AstNode *arg = node->args;
          FunctionDeclaration fun_decl;
          fun_decl.iteration = 0;
          fun_decl.num_arguments = 0;
          fun_decl.return_type = IdentifierToType(arg);
          arg = arg->next;
          strcpy(fun_decl.name, arg->token.string);
          arg = arg->next;

          for (; arg; arg = arg->next)
          {
            fun_decl.arg_types.push_back(IdentifierToType(arg));
            fun_decl.num_arguments++;
          }

          printf("Adding function declaration: %s\n", fun_decl.name);
          cenv->function_declarations.push_back(fun_decl);
        }
        break;
      case NODE_FUNCTION_DEFINITION:
        {
          bool found = false;
          for (auto &a : cenv->function_declarations)
          {
            if (strcmp(a.name, node->function_name.string) == 0)
            {
              found = true;
              break;
            }
          }

          if (!found)
          {
            FunctionDeclaration fun_decl;
            fun_decl.iteration = 0;
            fun_decl.num_arguments = 0;
            strcpy(fun_decl.name, node->function_name.string);

            printf("Adding function definition: %s\n", fun_decl.name);

            for (AstNode *arg = node->args; arg; arg = arg->next)
              fun_decl.num_arguments++;

            cenv->function_declarations.push_back(fun_decl);
          }
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

int InferTypes(AstNode *node, Scope *scope, CompileEnvironment *cenv, bool all)
{
  while (node)
  {
    switch (node->type)
    {
      case NODE_INTEGER:
        node->return_type = TYPE_I64;
        break;
      case NODE_FLOAT:
        node->return_type = TYPE_F64;
        break;
      case NODE_STRING:
        node->return_type = TYPE_U64;
        break;
      case NODE_IDENTIFIER:
        {
          const char *name = node->token.string;

          VariableDefinition *lexical_var = FindVariable(scope, name);
          if (lexical_var)
            node->return_type = lexical_var->type;
        }
        break;
      case NODE_SUBTRACT:
        if (!node->args->next)
        {
          node->return_type = InferTypes(node->args, scope, cenv, false);
          break;
        }
      case NODE_ADD:
      case NODE_MULTIPLY:
      case NODE_DIVIDE:
        {
          int a = InferTypes(node->args, scope, cenv, false);
          int b = InferTypes(node->args->next, scope, cenv, false);

          node->return_type = UprateTypes(a, b);
        }
        break;
      case NODE_FUNCALL:
        {
          InferTypes(node->args, scope, cenv, true);

          /* TODO return type of the function */
          node->return_type = TYPE_I64;
        }
        break;
      case NODE_ASSIGN:
        node->return_type = InferTypes(node->args2, scope, cenv, false);
        break;
      case NODE_DECLARE:
        node->return_type = TYPE_VOID;
        break;
      case NODE_FUNCTION_DEFINITION:
        node->return_type = TYPE_VOID;
        break;
      default:
        break;
    }

    if (!all)
      return node->return_type;

    if (!node->next)
      return node->return_type;

    node = node->next;
  }

  return TYPE_NONE;
}


llvm::Value *GenerateIr(AstNode *node, Scope *scope, CompileEnvironment *cenv, bool all)
{
  llvm::Value *return_value = nullptr;
  while (node)
  {
    switch (node->type)
    {
      case NODE_INTEGER:
        return_value = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*cenv->context), node->token.integer, false);
        break;
      case NODE_FLOAT:
        return_value = llvm::ConstantFP::get(*cenv->context, llvm::APFloat(node->token.floatval));
        break;
      case NODE_STRING:
        return_value = cenv->builder->CreateGlobalString(node->token.string);
        break;
      case NODE_ADD:
      case NODE_MULTIPLY:
      case NODE_SUBTRACT:
      case NODE_DIVIDE:
        {
          llvm::Value *a = GenerateIr(node->args, scope, cenv, false);
          llvm::Value *b = GenerateIr(node->args->next, scope, cenv, false);
          if (node->type == NODE_ADD)
            return_value = cenv->builder->CreateAdd(a, b);
          else if (node->type == NODE_MULTIPLY)
            return_value = cenv->builder->CreateMul(a, b);
          else if (node->type == NODE_SUBTRACT)
            return_value = cenv->builder->CreateSub(a, b);
          else if (node->type == NODE_DIVIDE)
            return_value = cenv->builder->CreateSDiv(a, b);
          // return_value = cenv->builder->CreateFDiv(a, b); TODO
        }
        break;
      case NODE_IDENTIFIER:
        {
          const char *name = node->token.string;

          VariableDefinition *lexical_var = FindVariable(scope, name);
          if (lexical_var)
          {
            return_value = lexical_var->value;
            break;
          }


          llvm::GlobalVariable *var = FindGlobalVariable(cenv, name, -1);
          if (var)
          {
            return_value = cenv->builder->CreateLoad(var->getValueType(), var, false /* volatile */);
            break;
          }

          CodeError(cenv->code_begin, cenv->code_end, node->token.pos, "Error - Variable \"%s\" not found\n", name);
          return NULL;
        }
        break;
      case NODE_DECLARE:
        {
          AstNode *arg = node->args;
          FunctionDeclaration fun_decl;
          fun_decl.iteration = 0;
          fun_decl.return_type = IdentifierToType(arg);
          arg = arg->next;
          strcpy(fun_decl.name, arg->token.string);
          arg = arg->next;

          for (; arg; arg = arg->next)
          {
            fun_decl.arg_types.push_back(IdentifierToType(arg));
          }

          cenv->function_declarations.push_back(fun_decl);

          /*Function *f =*/ AddFunctionDeclaration(&fun_decl, cenv->context.get(), cenv->mod.get());
        }
        break;
      case NODE_FUNCALL:
        {
          std::vector<llvm::Value *> args;
          for (AstNode *arg = node->args; arg; arg = arg->next)
          {
            args.push_back(GenerateIr(arg, scope, cenv, false));
          }

          llvm::Function *f = cenv->mod->getFunction(node->token.string);
          if (!f)
          {
#if 1
            FunctionDeclaration fun_decl;
            strcpy(fun_decl.name, node->token.string);
            fun_decl.return_type = TYPE_I64;
            for (AstNode *arg = node->args; arg; arg = arg->next)
            {
              fun_decl.arg_types.push_back(arg->return_type);
            }

            f = AddFunctionDeclaration(&fun_decl, cenv->context.get(), cenv->mod.get());
#else
            std::vector<Type *> types;
            for (auto &a : args)
            {
              types.push_back(a->getType());
            }

            FunctionType *ft = FunctionType::get(LlvmType(TYPE_I64, cenv->context.get()), types, false);
            f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, node->token.string, *cenv->mod);
#endif
          }
          if (f)
          {
            return_value = cenv->builder->CreateCall(f, args);
          }
          else
          {
            printf("Error: no function for name \"%s\" found, can't emit function call\n", node->token.string);
            return NULL;
          }
        }
        break;
      case NODE_FUNCTION_DEFINITION:
        {
          FunctionDeclaration fun_decl;
          fun_decl.iteration = 0;
          strcpy(fun_decl.name, node->function_name.string);
          for (auto &a : cenv->function_declarations)
          {
            if (strcmp(a.name, node->function_name.string) == 0)
            {
              snprintf(fun_decl.name, sizeof(fun_decl.name), "%s_%d", node->function_name.string, a.iteration++);
              cenv->patches.push_back(std::make_pair(strdup(a.name), strdup(fun_decl.name)));
              break;
            }
          }
          fun_decl.return_type = TYPE_I64;

          for (AstNode *arg = node->args; arg; arg = arg->next)
          {
            //types.push_back(llvm::Type::getInt64Ty(*cenv->context));
            fun_decl.arg_types.push_back(TYPE_I64);
          }
          //FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt64Ty(*cenv->context) /*return*/, types, false);
          //Function *f = cenv->mod->getFunction(node->function_name.string);

          llvm::Function *f = AddFunctionDeclaration(&fun_decl, cenv->context.get(), cenv->mod.get());
          llvm::BasicBlock *block = llvm::BasicBlock::Create(*cenv->context, "", f);
          auto previous_insert = cenv->builder->saveIP();
          cenv->builder->SetInsertPoint(block);
          cenv->function_declarations.push_back(fun_decl);

          Scope s;
          {
            AstNode *arg = node->args;
            for (auto &a : f->args())
            {
              /* get Value types for arguments */
              a.setName(arg->token.string);

              VariableDefinition variable_definition;
              strlcpy(variable_definition.name, arg->token.string, sizeof(variable_definition.name));
              variable_definition.value = &a;
              variable_definition.type = arg->return_type;

              s.variables.push_back(variable_definition);

              arg = arg->next;
            }
          }

          /* generate IR for the function body */
          llvm::Value *ret_val = GenerateIr(node->args2, &s, cenv, true);
          if (ret_val)
          {
            cenv->builder->CreateRet(ret_val);
            llvm::verifyFunction(*f);

            /* optimize code */
#if 0
            TheFPM->run(*f, *TheFAM);
#endif
          }
          f->print(llvm::errs());
          cenv->builder->restoreIP(previous_insert);

        }
        break;
      case NODE_ASSIGN:
        {
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

            llvm::Value *val = GenerateIr(node->args2, scope, cenv, false);
            cenv->builder->CreateStore(val, var, false /* volatile */);
          }
          else
          {
            /* lexical variable */
          }
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

int main(int argc, char *argv[])
{
  llvm::InitLLVM raii_1(argc, argv); /* helper class for stacktrace, utf-8 argv etc. */

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  auto jit = llvm::orc::LLJITBuilder().create();

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

    file = (i8 *)calloc(1, len + 1);
    file[0] = '\0';
    if (fread(file, 1, len, f) != len)
    {
      printf("Failed to read %ld bytes from file \"%s\".\n", len, filename);
    }
    file[len] = '\0';

    fclose(f);
  }

  while (true)
  {
    printf("> "); fflush(stdin);
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
    PrintAstNode(root_node);

    /* compilation */
    cenv.code_begin = tokenizer_state.begin;
    cenv.code_end = tokenizer_state.end;

    CollectDeclarations(root_node, nullptr, &cenv, true);

    int toplevel_return_type = InferTypes(root_node, nullptr, &cenv, true);

    /* compile to LLVM IR */
    /* create module for this compilation unit */
    snprintf(cenv.module_name, sizeof(cenv.module_name), "m%03i", inputi);
    snprintf(cenv.toplevel_function_name, sizeof(cenv.toplevel_function_name), "m%03i_toplevel", inputi);

    /* init LLVM and create a module */
    cenv.context = std::make_unique<llvm::LLVMContext>(); /* 1 obj per thread */
    cenv.builder = new llvm::IRBuilder<>(*cenv.context);
    cenv.mod = std::make_unique<llvm::Module>(cenv.module_name, *cenv.context);

    /* add existing function declarations */
    for (auto &a : cenv.function_declarations)
    {
      AddFunctionDeclaration(&a, cenv.context.get(), cenv.mod.get());
    }

    FunctionDeclaration toplevel_function;
    strcpy(toplevel_function.name, cenv.toplevel_function_name);
    toplevel_function.return_type = toplevel_return_type;

    llvm::Function *f = AddFunctionDeclaration(&toplevel_function, cenv.context.get(), cenv.mod.get());
    llvm::BasicBlock *block = llvm::BasicBlock::Create(*cenv.context, "", f);
    cenv.builder->SetInsertPoint(block);

    llvm::Value *ret_val = GenerateIr(root_node, NULL, &cenv, true);

    cenv.builder->CreateRet(ret_val);
    llvm::verifyFunction(*f);

    /* optimize code */
#if 0
    TheFPM->run(*f, *TheFAM);
#endif

    cenv.mod->dump();

    /* this will compile IR code in the module into machine code */
    auto r1 = jit->get()->addIRModule(llvm::orc::ThreadSafeModule(std::move(cenv.mod), std::move(cenv.context)));

    for (auto &a : cenv.patches)
    {
      printf("Patching %s -> %s\n", a.first, a.second);
      auto func_addr = jit->get()->lookup(a.first);
      auto func_addr2 = jit->get()->lookup(a.second);
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
        printf("Failed to get function addresses\n");
      }
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

    FreeAstNode(root_node);

    if (file)
      break;
  }

  return 0;
}

