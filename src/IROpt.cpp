#include "IROpt.hpp"

#include <iostream>

// Opt
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"

#include "llvm/IR/LegacyPassManagers.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

constexpr unsigned int str2int(const char* str, int h = 0) {
  return !str[h] ? 5381 : (str2int(str, h+1) * 33) ^ str[h];
}

void grc::IROpt::populateFuncPassManager(llvm::legacy::FunctionPassManager* FPM, std::vector<std::string> PassesNames) {
  for (std::string PassName : PassesNames) {
    switch (str2int(PassName.c_str())) {
      //case str2int("instcombine"):
      //  FPM->add(llvm::createInstructionCombiningPass());
      //  break;
      case str2int("simplifycfg"):
        FPM->add(llvm::createCFGSimplificationPass());
        break;
      case str2int("reassociate"):
        FPM->add(llvm::createReassociatePass());
        break;
      case str2int("gvn"):
        FPM->add(llvm::createNewGVNPass());
        break;
      case str2int("die"):
        FPM->add(llvm::createDeadInstEliminationPass());
        break;
      case str2int("dce"):
        FPM->add(llvm::createDeadCodeEliminationPass());
        break;
//      case str2int("mem2reg"):
//        FPM->add(llvm::createPromoteMemoryToRegisterPass());
//        break;
      case str2int("licm"):
        FPM->add(llvm::createLICMPass());
        break;
      case str2int("memcpyopt"):
        FPM->add(llvm::createMemCpyOptPass());
        break;
      case str2int("loop-unswitch"):
        FPM->add(llvm::createLoopUnswitchPass());
        break;
      case str2int("indvars"):
        FPM->add(llvm::createIndVarSimplifyPass());       // Canonicalize indvars
        break;
      case str2int("loop-deletion"):
        FPM->add(llvm::createLoopDeletionPass());         // Delete dead loops
        break;
      case str2int("loop-predication"):
        FPM->add(llvm::createLoopPredicationPass());
        break;
      case str2int("loop-unroll"):
        FPM->add(llvm::createSimpleLoopUnrollPass());     // Unroll small loops
        break;
      default:
        std::cerr << "Trying to use an invalid optimization pass!\n";
        exit(1);
        break;
    }
  }
}

void grc::IROpt::optimizeIRFunction(llvm::Module *M, OptLevel Level) {
  // Lazy initialization
  if (Level == OptLevel::Basic) {
    if (!BasicPM) {
      BasicPM = std::make_unique<llvm::legacy::FunctionPassManager>(M);
      populateFuncPassManager(BasicPM.get(), 
        {"simplifycfg", "reassociate", "gvn", "die", "dce", "licm", 
        "memcpyopt", "loop-unswitch", "indvars", "loop-deletion", "loop-predication", "loop-unroll" , 
        "simplifycfg", "licm", "gvn"});
      BasicPM->doInitialization();
    }
    for (auto &F : *M)
      BasicPM->run(F);
  } 
}
