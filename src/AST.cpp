#include "AST.hpp"

using namespace grc;

/***********
 * others  *
 ***********/
void BlockAST::addExprAST(std::unique_ptr<ExprAST> e) {
  exps.push_back(std::move(e));
}
/***********
 *   log   *
 ***********/
void NumberExprAST::toPrint() {
  std::cout << Val << ' ';
};

void BooleanExprAST::toPrint() {
  if(Bool)
    std::cout << "True ";
  else
    std::cout << "False ";
}

void VariableExprAST::toPrint() {
  std::cout << Name << ' ';
}

void UnaryExprAST::toPrint() {
  std::cout << Op << ' '; 
  Operand->toPrint();
}

void BinaryExprAST::toPrint() {
  std::cout << Op << ' ';
  LHS->toPrint();
  RHS->toPrint();
}

void IfExprAST::toPrint() {
  Cond->toPrint();
  Then->toPrint();
}

void BlockAST::toPrint() {
  for(int i = 0; i < exps.size(); i++) {
    exps[i]->toPrint();
    std::cout << "\n";
  }
}
/***********
 * codegen *
 ***********/
llvm::Value* ProcedureAST::codegen(llvm::LLVMContext &TheContext) {
  int e = 5;
  return llvm::ConstantInt::get(TheContext, llvm::APInt(32, e));
}

//llvm::GlobalVariable* DecVarAST::codegen(llvm::LLVMContext &TheContext) {
//  llvm::GlobalVariable::get(TheContext, llvm::Type::IntegerType, );
//  return nullptr;
//}
