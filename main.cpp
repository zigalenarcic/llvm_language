#include "compiler.h"

#include "llvm/IR/Verifier.h"
/* JIT */
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"

#define PROMPT "$ "

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
  int inputi = -1;
  CompileEnvironment cenv;
  AstNode *full_ast = nullptr;
  AstNode *full_ast_last = nullptr;
  i8 *file = NULL;

  llvm::InitLLVM raii_1(argc, argv); /* helper class for stacktrace, utf-8 argv etc. */

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  auto jit = llvm::orc::LLJITBuilder().create();
  if (!jit)
  {
    fprintf(stderr, "Failed to create a LLJIT object.\n");
    exit(EXIT_FAILURE);
  }

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

  while (true)
  {
    if (!file)
    {
      printf(PROMPT); fflush(stdin);
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
      /* compile AST to LLVM IR */
      /* create module for this compilation unit */
      snprintf(cenv.module_name, sizeof(cenv.module_name), "m%03i", inputi);
      snprintf(cenv.toplevel_function_name, sizeof(cenv.toplevel_function_name), "m%03i_toplevel", inputi);

      /* init LLVM and create a module */
      cenv.context = std::make_unique<llvm::LLVMContext>(); /* 1 obj per thread */
      cenv.builder = new llvm::IRBuilder<>(*cenv.context);
      cenv.mod = std::make_unique<llvm::Module>(cenv.module_name, *cenv.context);

      ValueType ret = GenerateIr(root_node, NULL, &cenv, true, false /* emit_ir */, false);
      printf("toplevel ret type %s\n", TypeToString(ret.second));
      int toplevel_return_type = ret.second == TYPE_UNKNOWN ? TYPE_VOID : ret.second;

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
            {
              u64 code_result = ((u64 (*)())func_addr->getValue())();
              printf("Result u64 = %lu (0x%016lx)\n", code_result, code_result);
            }
            break;
          case TYPE_I64:
            {
              i64 code_result = ((i64 (*)())func_addr->getValue())();
              printf("Result i64 = %ld (0x%016lx)\n", code_result, (u64)code_result);
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

