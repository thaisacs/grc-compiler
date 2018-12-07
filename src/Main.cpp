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
#include <unistd.h>
#include <getopt.h>
#include "Scope.hpp"
#include "Log.hpp"
#include "IROpt.hpp"

using namespace llvm;
using namespace llvm::sys;

extern int yyparse();
extern FILE *yyin;
extern bool isError;

llvm::LLVMContext TheContext;
std::unique_ptr<llvm::Module> TheModule;
std::shared_ptr<grc::Scope> S = std::make_shared<grc::Scope>();
std::unique_ptr<grc::Log> LOG;

char FileName[150];
bool isError, doOpt, doLog, doView;

void InitializeModuleAndPassManager() {
  // Open a new Module
  TheModule = llvm::make_unique<llvm::Module>("grc-compiler", TheContext);
}

void usage() {
  std::cout << "Usage: grcc [options] -f <filename>\n";
  std::cout << "Options:\n";
  std::cout << "  -h        \t Display this information.\n";
  std::cout << "  -O        \t Active optimization.\n";
  std::cout << "  -l        \t Write scopes in file 'GRCLog.out'.\n";
  std::cout << "  -v        \t Display IR\n";
}

void OptRun() {
  std::unique_ptr<grc::IROpt> IRO = llvm::make_unique<grc::IROpt>();
  IRO->optimizeIRFunction(TheModule.get(), grc::IROpt::OptLevel::Basic);
}

void config(int argc, char *argv[]) {
  int op;
  bool verifyFile = false;

  isError = false;
  doOpt = false;
  doLog = false;
  doView = false;

  if(argc == 1) {
    std::cerr << "invalid arguments\n";
    exit(1);
  }
  
  while((op = getopt(argc, argv, "Olvhf:")) > 0) {
    switch(op) {
      case 'h':
        usage();
        exit(0);
        break;
      case 'O':
        doOpt = true;
        break;
      case 'l':
        doLog = true;
        break;
      case 'f':
        verifyFile = true;
        std::strcpy(FileName, optarg);
        break;
      case 'v':
        doView = true;
        break;
      default:
        exit(0);
    }
  }

  if(!verifyFile) {
    std::cerr << "You must inform the file\n";
    exit(0);
  }
}

int main(int argc, char *argv[]) {
  std::error_code EC;
  
  config(argc, argv);

  FILE *i = fopen(FileName, "r");
  if(!i) {
    std::cerr << "File '" << FileName << "' not found\n";
    exit(0);
  }
  
  InitializeModuleAndPassManager();
  
  if(doLog) {
    LOG = std::make_unique<grc::Log>("GRCLog.out");
  }

  S->initializeScope();

  yyin = i;
  yyparse();

  S->finalizeScope();
  
  if(!isError) {
    if(doView)
      TheModule->print(errs(), nullptr);
  
    if(doOpt)
      OptRun(); 

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    auto TargetTriple = sys::getDefaultTargetTriple();
    TheModule->setTargetTriple(TargetTriple);

    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

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

    Function *Main = TheModule->getFunction("main");
    if(Main) {
      if(doOpt && doView) {
        std::cout << "=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#\n";
        TheModule->print(errs(), nullptr);
      }
      
      system("clang-7 prog.o");
    }else {
      std::cerr << "'main' function not found" << std::endl;
    }
  } 
  return 0;
}
