#include "AST.hpp"

using namespace grc;

llvm::Value* ProcedureAST::codegen(llvm::LLVMContext &TheContext) {
  int e = 5;
  return llvm::ConstantInt::get(TheContext, llvm::APInt(32, e));
}

//llvm::GlobalVariable* DecVarAST::codegen(llvm::LLVMContext &TheContext) {
//  llvm::GlobalVariable::get(TheContext, llvm::Type::IntegerType, );
//  return nullptr;
//}
