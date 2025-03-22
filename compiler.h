#ifndef _COMPILER_H_
#define _COMPILER_H_

#include "parser.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"

typedef std::pair<llvm::Value *, int> ValueType;

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


const char *TypeToString(int type);
llvm::Function *AddFunctionDeclarationToModule(const char *name, int return_type, std::vector<int> &arg_types, llvm::LLVMContext *context, llvm::Module *mod);
int CollectDeclarations(AstNode *node, Scope *scope, CompileEnvironment *cenv, bool all);
ValueType GenerateIr(AstNode *node, Scope *scope, CompileEnvironment *cenv, bool all, bool emit_ir, bool definitions = false);

bool PatchFunctions(llvm::orc::LLJIT *jit, const char *name1, const char *name2);
#endif // _COMPILER_H_

