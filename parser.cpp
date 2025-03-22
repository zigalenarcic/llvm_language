#include <cstdio>
#include <cstdarg>
#include <cstring>
#include <cmath>

#include "parser.h"

///////////////////////////////////////////////////////////////////////////////
// 1 PARSER
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// 1.1 LEXER
///////////////////////////////////////////////////////////////////////////////

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
  { "if", TOK_IF },
  { "for", TOK_FOR },
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

///////////////////////////////////////////////////////////////////////////////
// 1.2 PARSER
///////////////////////////////////////////////////////////////////////////////

void InitParser(TokenizerState *state)
{
  state->t1 = GetToken(state);
  state->paren_count = 0;
}

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

