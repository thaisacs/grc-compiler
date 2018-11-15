#pragma once

#include "llvm/IR/Module.h"
#include "llvm/IR/LegacyPassManager.h"

namespace grc {
  class IROpt {
    std::unique_ptr<llvm::legacy::FunctionPassManager> BasicPM;
    void populateFuncPassManager(llvm::legacy::FunctionPassManager*, std::vector<std::string>);
  public:
    IROpt() {}; 
    enum OptLevel { Basic, Soft, Medium, Hard, Custom };
    void optimizeIRFunction(llvm::Module*, OptLevel);
    void customOptimizeIRFunction(llvm::Module*, std::vector<std::string>);
  };
}
