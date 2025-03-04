#include <cstdio>
#include <cstdarg>
#include <cstring>
#include <cmath>

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

  TOK_PLUS,
  TOK_MINUS,
  TOK_TIMES,
  TOK_DIV,

  TOK_EQUALS,
  TOK_COMMA,
  TOK_COLON,
  TOK_SEMICOLON,
};

typedef struct Token_
{
  int type;
  char string[64];
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
        return t;
      case STATE_STRING:
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
        t.integer = atoi(state->buffer);
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
};

typedef struct AstNode {
  int type;
  Token token;
  struct AstNode *args;

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

void IntepreterError(TokenizerState *state, int pos, const char *fmt, ...)
{
  char tmp[1024];
  va_list list;
  va_start(list, fmt);
  vsnprintf(tmp, sizeof(tmp), fmt, list);
  va_end(list);

  int line = 1;
  const char *line_start = state->begin;

  const char *it = state->begin;

  while (it != state->end && (it - state->begin) < pos)
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
  while (it != state->end)
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
      case TOK_MINUS:
        if (!expr) /* allow single argument negation */
        {
          AstNode *node1 = ParseExpression(state, end_token, -1, 15);

          if (!node1)
          {
            IntepreterError(state, t1.pos, "Missing argument\n");
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
            IntepreterError(state, t1.pos, "Unexpected token\n");
            return NULL;
          }
          AstNode *node1 = expr;
          AstNode *node2 = ParseExpression(state, end_token, -1, GetPrecedence(t1.type));

          if (!node2)
          {
            IntepreterError(state, t1.pos, "Failed to parse second operand\n");
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
          IntepreterError(state, t1.pos, "Empty parenthesis\n");
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
            IntepreterError(state, t1.pos, "Unexpected token\n");
            return NULL;
          }

          expr = MakeAstNode(NODE_INTEGER, t1);
        }
        break;
      case TOK_FLOAT:
        {
          if (expr)
          {
            IntepreterError(state, t1.pos, "Unexpected token\n");
            return NULL;
          }

          expr = MakeAstNode(NODE_FLOAT, t1);
        }
        break;
      case TOK_STRING:
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
              IntepreterError(state, t1.pos, "Unexpected token\n");
              return NULL;
            }

            expr = MakeAstNode(NODE_IDENTIFIER, t1);
          }
        }
        break;
      default:
        IntepreterError(state, t1.pos, "Unexpected token\n");
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
      IntepreterError(state, t1.pos, "Missing token\n");
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

const char *StringType(int type)
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
  }
  return "?";
}

void PrintAstNode1(AstNode *node, int level)
{
  while (node != NULL)
  {
    for (int i = 0; i < level; i++)
      printf("  ");
    printf("Node: %d (%s)", node->type, StringType(node->type));
    switch (node->type)
    {
      case NODE_INTEGER:
        printf(" value %lu", node->token.integer);
        break;
      case NODE_FLOAT:
        printf(" value %f", node->token.floatval);
        break;
    }
    printf("\n");
    if (node->args)
      PrintAstNode1(node->args, level + 1);

    node = node->next;
  }
}

void PrintAstNode(AstNode *node)
{
  PrintAstNode1(node, 0);
}

void FreeAstNode(AstNode *node)
{
  while (node != NULL)
  {
    if (node->args)
      FreeAstNode(node->args);

    AstNode *next = node->next;

    delete node;

    node = next;
  }
}

int main(int argc, char *argv[])
{
  while (true)
  {
    printf("> "); fflush(stdin);

    char line[1024];
    if (fgets(line, sizeof(line), stdin) == NULL)
      return 0;

    TokenizerState tokenizer_state;

    InitTokenizer(&tokenizer_state, line);

    InitParser(&tokenizer_state);
    AstNode *node = ParseToplevel(&tokenizer_state);
    PrintAstNode(node);
    FreeAstNode(node);
  }

  return 0;
}

