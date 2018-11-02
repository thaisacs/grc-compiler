#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <iostream>
#include <memory>

extern int yyparse();
extern FILE *yyin;

llvm::LLVMContext TheContext;
std::unique_ptr<llvm::Module> TheModule;

int main(int argc, char *argv[]) {
  TheModule = llvm::make_unique<llvm::Module>("grc-compiler", TheContext);
  
  if(argc < 2)
    exit(1);
  
  FILE *i = fopen(argv[1], "r");
  yyin = i;
  
  yyparse();
  
  //TheModule->print(llvm::errs(), nullptr);
  
  return 0;
}
