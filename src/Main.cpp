#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

#include <iostream>
#include <memory>

#include "Scope.hpp"
#include "Log.hpp"

extern int yyparse();
extern FILE *yyin;

llvm::LLVMContext TheContext;
std::unique_ptr<llvm::Module> TheModule;

std::shared_ptr<grc::Scope> S = std::make_shared<grc::Scope>();
std::unique_ptr<grc::Log> LOG = std::make_unique<grc::Log>("GRCLog.out");

int main(int argc, char *argv[]) {
  TheModule = llvm::make_unique<llvm::Module>("grc-compiler", TheContext);
  
  if(argc < 2)
    exit(1);
  
  FILE *i = fopen(argv[1], "r");
  
  S->initializeScope();

  yyin = i;
  yyparse();
  
  TheModule->print(llvm::errs(), nullptr);
  
  LOG->scopes(S);
  
  S->finalizeScope();
  
  return 0;
}
