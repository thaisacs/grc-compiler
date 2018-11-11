#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <iostream>
#include <memory>

#include "Scope.hpp"
#include "Log.hpp"

using namespace llvm;
using namespace llvm::sys;

extern int yyparse();
extern FILE *yyin;

llvm::LLVMContext TheContext;
std::unique_ptr<llvm::Module> TheModule;

std::shared_ptr<grc::Scope> S = std::make_shared<grc::Scope>();
std::unique_ptr<grc::Log> LOG = std::make_unique<grc::Log>("GRCLog.out");

void InitializeModuleAndPassManager() {
  // Open a new Module
  TheModule = llvm::make_unique<llvm::Module>("grc-compiler", TheContext);
}

int main(int argc, char *argv[]) {
  if(argc < 2)
    exit(1);
  
  FILE *i = fopen(argv[1], "r");
  
  InitializeModuleAndPassManager();
  S->initializeScope();
  
  yyin = i;
  yyparse();
  
  
  LOG->scopes(S);
  S->finalizeScope();
  
  // Initialize the target registry etc.
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();
  
  auto TargetTriple = sys::getDefaultTargetTriple();
  TheModule->setTargetTriple(TargetTriple);

  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    errs() << Error;
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = Optional<Reloc::Model>();
  auto TheTargetMachine =
      Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

  TheModule->setDataLayout(TheTargetMachine->createDataLayout());

  auto Filename = "prog.o";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::F_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }

  legacy::PassManager pass;
  auto FileType = TargetMachine::CGFT_ObjectFile;

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    return 1;
  }

  pass.run(*TheModule);
  dest.flush();

  TheModule->print(llvm::errs(), nullptr);
  
  //outs() << "Wrote " << Filename << "\n";

  //system("clang-7 prog.o");

  return 0;
}
