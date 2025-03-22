///////////////////////////////////////////////////////////////////////////////
// 2 COMPILER
///////////////////////////////////////////////////////////////////////////////

/* building IR */
#include "llvm/ADT/APFloat.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Type.h"

#include "compiler.h"

#include <vector>
#include <sys/mman.h>

typedef struct {
  int type;
  const char *name;
  const char *language_name;
  int bits;
  bool floating_point;
  bool sign;
  int alt_type;
} TypeDescription;

TypeDescription type_descriptions[] = {
  { TYPE_UNKNOWN, "unknown", "-", 0, false, false, TYPE_UNKNOWN},
  { TYPE_VOID, "void", "void", 0, false, false, TYPE_VOID},
  { TYPE_I64, "i64", "i64", 64, false, true, TYPE_U64},
  { TYPE_U64, "u64", "u64", 64, false, false, TYPE_I64},
  { TYPE_F64, "f64", "f64", 64, true, true, TYPE_F64},
  { TYPE_F32, "f32", "f32", 32, true, true, TYPE_F64},
  { TYPE_U8, "u8", "u8", 8, false, false, TYPE_I8},
  { TYPE_I8, "i8", "i8", 8, false, true, TYPE_U8},
  { TYPE_U16, "u16", "u16", 16, false, false, TYPE_I16},
  { TYPE_I16, "i16", "i16", 16, false, true, TYPE_U16},
  { TYPE_U32, "u32", "u32", 32, false, false, TYPE_I32},
  { TYPE_I32, "i32", "i32", 32, false, true, TYPE_U32},
  { TYPE_PTR, "ptr", "ptr", 64, false, false, TYPE_PTR},
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
    for (int i = 0; i < sizeof(type_descriptions) / sizeof(type_descriptions[0]); i++)
    {
      if (strcmp(type_descriptions[i].language_name, arg->token.string) == 0)
        return type_descriptions[i].type;
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

bool AddIfMissingDeclare(CompileEnvironment *cenv, FunctionInfo *fi)
{
  for (auto &a : cenv->functions_to_declare)
    if (strcmp(a->name, fi->name) == 0)
      return false;

  cenv->functions_to_declare.push_back(fi);
  return true;
}

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
            AddIfMissingDeclare(cenv, fi);
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

ValueType GenerateIr(AstNode *node, Scope *scope, CompileEnvironment *cenv, bool all, bool emit_ir, bool definitions)
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

            AddIfMissingDeclare(cenv, fi);
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

