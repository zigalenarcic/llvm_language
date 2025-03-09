/* building IR */
#include "llvm/ADT/APFloat.h"
// #include "llvm/ADT/STLExtras.h"
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
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
/* JIT */
#include "llvm/Support/InitLLVM.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"

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

  TOK_DEF,
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

struct
{
  const char *str;
  int type;
} specializations[] =
{
  { "def", TOK_DEF },
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

  NODE_DEF,
  NODE_LIST,
};

typedef struct AstNode {
  int type;
  Token token;
  struct AstNode *args;

  /* function data */
  Token function_name;
  struct AstNode *arglist;

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
      case TOK_DEF:
        {
          Token t2 = PopToken(state);
          if (t2.type != TOK_IDENTIFIER)
          {
            IntepreterError(state, t2.pos, "Missing identifier\n");
          }
          expr = MakeAstNode(NODE_DEF, t1);
          expr->function_name = t2;

          Token t3 = PopToken(state);
          if (t3.type != TOK_LEFT_PAREN)
          {
            IntepreterError(state, t3.pos, "Missing left parenthesis\n");
          }
          ParseList(&expr->arglist, state, TOK_COMMA, TOK_RIGHT_PAREN);

          Token t4 = PopToken(state);
          if (t4.type != TOK_LEFT_BRACE)
          {
            IntepreterError(state, t4.pos, "Missing left brace\n");
          }
          ParseBody(&expr->args, state);

          return expr;
        }
        break;
      case TOK_MINUS:
        if (!expr) /* allow single argument negation */
        {
          AstNode *node1 = ParseExpression(state, end_token, end_token2, 15);

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
          AstNode *node2 = ParseExpression(state, end_token, end_token2, GetPrecedence(t1.type));

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
      IntepreterError(state, t1.pos, "Missing token\n");
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
    case NODE_DEF: return "DEF";
    case NODE_LIST: return "LIST OF";
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
      case NODE_IDENTIFIER:
        printf(" value %s", node->token.string);
        break;
      case NODE_DEF:
        printf(" function name \"%s\" arglist:\n", node->function_name.string);
        if (node->arglist)
          PrintAstNode1(node->arglist, level + 1);
        for (int i = 0; i < level; i++)
          printf("  ");
        printf("    function body:");
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

using namespace llvm;
using namespace llvm::orc;

std::unique_ptr<FunctionPassManager> TheFPM;
std::unique_ptr<LoopAnalysisManager> TheLAM;
std::unique_ptr<FunctionAnalysisManager> TheFAM;
std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
std::unique_ptr<ModuleAnalysisManager> TheMAM;
std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
std::unique_ptr<StandardInstrumentations> TheSI;

typedef enum {
  TYPE_NONE = 0,
  TYPE_I64,
  TYPE_U64,
  TYPE_F64,
} ETypes;

typedef struct FunctionDeclaration {
  char name[128];
  ETypes return_type;
  std::vector<ETypes> arg_types;
} FunctionDeclaration;

Type *LlvmType(ETypes type, LLVMContext *context)
{
  switch (type)
  {
    case TYPE_NONE:
      return NULL;
      break;
    case TYPE_I64:
    case TYPE_U64:
    default:
      return Type::getInt64Ty(*context);
      break;
    case TYPE_F64:
      return Type::getDoubleTy(*context);
      break;
  }
}

const char *TypeToString(ETypes type)
{
  switch (type)
  {
    case TYPE_NONE: return "NONE";
    case TYPE_I64: return "I64";
    case TYPE_U64: return "U64";
    case TYPE_F64: return "F64";
    default: return "UNKNOWN";
  }
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

Function *AddFunctionDeclaration(FunctionDeclaration *fun_decl, LLVMContext *context, Module *mod)
{
  printf("Adding function declaration: ");
  PrintFunctionDeclaration(fun_decl);

  std::vector<Type *> types;
  for (auto &a : fun_decl->arg_types)
  {
    types.push_back(LlvmType(a, context));
  }
  FunctionType *ft = FunctionType::get(LlvmType(fun_decl->return_type, context), types, false);
  //Function *f = mod->getFunction(node->function_name.string);
  Function *f = Function::Create(ft, Function::ExternalLinkage, fun_decl->name, *mod);

  return f;
}

typedef struct CompileEnvironment {
  std::unique_ptr<LLVMContext> context;
  IRBuilder<> *builder;
  std::unique_ptr<Module> mod;

  char module_name[64];
  char toplevel_function_name[64];

  std::vector<FunctionDeclaration> function_declarations;
} CompileEnvironment;

typedef struct Scope {
  std::vector<Value *> values;
  std::vector<char *> names;
} Scope;

Value *FindVar(Scope *scope, const char *name)
{
  if (!scope)
  {
    return NULL;
  }

  for (int i = scope->names.size() - 1; i >= 0; i--)
  {
    if (strcmp(scope->names[i], name) == 0)
    {
      return scope->values[i];
    }
  }

  return NULL;
}

Value *GenerateIr(AstNode *node, Scope *scope, bool toplevel, CompileEnvironment *cenv);

Value *GenerateIrList(AstNode *node, Scope *scope, bool toplevel, CompileEnvironment *cenv)
{
  Value *ret_val = NULL;
  while (node)
  {
    ret_val = GenerateIr(node, scope, toplevel, cenv);
    node = node->next;
  }

  return ret_val;
}

Value *GenerateIr(AstNode *node, Scope *scope, bool toplevel, CompileEnvironment *cenv)
{
  switch (node->type)
  {
    case NODE_INTEGER:
      return ConstantInt::get(Type::getInt64Ty(*cenv->context), node->token.integer, false);
      break;
    case NODE_FLOAT:
      return ConstantFP::get(*cenv->context, APFloat(node->token.floatval));
      break;
    case NODE_ADD:
    case NODE_MULTIPLY:
    case NODE_SUBTRACT:
    case NODE_DIVIDE:
      {
        Value *a = GenerateIr(node->args, scope, toplevel, cenv);
        Value *b = GenerateIr(node->args->next, scope, toplevel, cenv);
        if (node->type == NODE_ADD)
          return cenv->builder->CreateAdd(a, b);
        else if (node->type == NODE_MULTIPLY)
          return cenv->builder->CreateMul(a, b);
        else if (node->type == NODE_SUBTRACT)
          return cenv->builder->CreateSub(a, b);
        else if (node->type == NODE_DIVIDE)
          return cenv->builder->CreateSDiv(a, b);
      }
      break;
    case NODE_IDENTIFIER:
      {
        return FindVar(scope, node->token.string);
      }
      break;
    case NODE_FUNCALL:
      {
        Function *f = cenv->mod->getFunction(node->token.string);
        if (f)
        {
          std::vector<Value *> args;
          AstNode *n = node->args;
          while (n)
          {
            args.push_back(GenerateIr(n, scope, toplevel, cenv));
            n = n->next;
          }

          //cenv->builder->CreateCall(f->getFunctionType(), NULL, args);
          return cenv->builder->CreateCall(f, args);
        }
        else
        {
          printf("Error: no function for name \"%s\" found, can't emit function call\n", node->token.string);
          return NULL;
        }
      }
      break;
    case NODE_DEF:
      {
        //std::vector<Type *> types;
        /* create vector with types */
        //types.push_back(Type::getDoubleTy(*cenv->context));
        AstNode *arg = node->arglist;
        
        FunctionDeclaration fun_decl;
        strcpy(fun_decl.name, node->function_name.string);
        fun_decl.return_type = TYPE_I64;

        while (arg)
        {
          //types.push_back(Type::getInt64Ty(*cenv->context));
          arg = arg->next;
          fun_decl.arg_types.push_back(TYPE_I64);
        }
        //FunctionType *ft = FunctionType::get(Type::getInt64Ty(*cenv->context) /*return*/, types, false);
        //Function *f = cenv->mod->getFunction(node->function_name.string);

        Function *f = AddFunctionDeclaration(&fun_decl, cenv->context.get(), cenv->mod.get());
        BasicBlock *block = BasicBlock::Create(*cenv->context, "body", f);
        auto previous_insert = cenv->builder->saveIP();
        cenv->builder->SetInsertPoint(block);
        cenv->function_declarations.push_back(fun_decl);

        Scope s;
        {
          AstNode *arg = node->arglist;
          for (auto &a : f->args())
          {
            /* get Value types for arguments */
            a.setName(arg->token.string);

            s.values.push_back(&a);
            s.names.push_back(arg->token.string);

            arg = arg->next;
          }
        }

        /* generate IR for the function body */
        Value *ret_val = GenerateIrList(node->args, &s, false, cenv);
        if (ret_val)
        {
          cenv->builder->CreateRet(ret_val);
          verifyFunction(*f);

          /* optimize code */
#if 0
          TheFPM->run(*f, *TheFAM);
#endif
        }
        f->print(errs());
        cenv->builder->restoreIP(previous_insert);

      }
      break;
  }

  return NULL;
}

int main(int argc, char *argv[])
{
  InitLLVM raii_1(argc, argv); /* helper class for stacktrace, utf-8 argv etc. */

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  // llvm::InitializeNativeTargetDisassmebler();

  auto jit = LLJITBuilder().create();

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

  while (true)
  {
    printf("> "); fflush(stdin);
    inputi++;

    char line[1024];
    if (fgets(line, sizeof(line), stdin) == NULL)
      return 0;

    TokenizerState tokenizer_state;

    InitTokenizer(&tokenizer_state, line);

    InitParser(&tokenizer_state);
    AstNode *node = ParseToplevel(&tokenizer_state);
    PrintAstNode(node);

    /* compile */
    /* create module for this compilation unit */
    snprintf(cenv.module_name, sizeof(cenv.module_name), "mod%02i", inputi);
    snprintf(cenv.toplevel_function_name, sizeof(cenv.toplevel_function_name), "mod%02i_toplevel", inputi);

    /* init LLVM */
    cenv.context = std::make_unique<LLVMContext>(); /* 1 obj per thread */
    cenv.builder = new IRBuilder<>(*cenv.context);
    cenv.mod = std::make_unique<Module>(cenv.module_name, *cenv.context);

    /* add existing function declarations */
    for (auto &a : cenv.function_declarations)
    {
      AddFunctionDeclaration(&a, cenv.context.get(), cenv.mod.get());
    }

    FunctionDeclaration toplevel_function;
    strcpy(toplevel_function.name, cenv.toplevel_function_name);
    toplevel_function.return_type = TYPE_I64;

    Function *f = AddFunctionDeclaration(&toplevel_function, cenv.context.get(), cenv.mod.get());
    BasicBlock *block = BasicBlock::Create(*cenv.context, "body", f);
    cenv.builder->SetInsertPoint(block);

    Value *ret_val = GenerateIrList(node, NULL, true, &cenv);

    if (ret_val)
    {
      cenv.builder->CreateRet(ret_val);
      verifyFunction(*f);

      /* optimize code */
#if 0
      TheFPM->run(*f, *TheFAM);
#endif
    }
    else
    {
      cenv.builder->CreateRet(ConstantInt::get(Type::getInt64Ty(*cenv.context), 0, false));
      verifyFunction(*f);
    }
    f->print(errs());

    auto r1 = jit->get()->addIRModule(ThreadSafeModule(std::move(cenv.mod), std::move(cenv.context)));

    auto func_addr = jit->get()->lookup(cenv.toplevel_function_name);
    if (func_addr)
    {
      i64 (*fun)() = func_addr->toPtr<i64()>();
      i64 code_result = fun();
      printf("Result = %ld\n", code_result);
    }

    FreeAstNode(node);
  }

  return 0;
}

