#include "AST.hpp"

using namespace grc;

//===----------------------------------------------------------------------===//
//// NumberExprAST 
////===----------------------------------------------------------------------===//

void NumberExprAST::toPrint(std::ofstream &File, Traversals T) {
  File << Val;
};

llvm::Value* NumberExprAST::codegen(llvm::LLVMContext &TheContext) {
  return llvm::ConstantInt::get(TheContext, llvm::APInt(32, Val));
}

//===----------------------------------------------------------------------===//
//// BooleanExprAST 
////===----------------------------------------------------------------------===//

void BooleanExprAST::toPrint(std::ofstream &File, Traversals T) {
  if(Bool)
    File << "True";
  else
    File << "False";
}

//===----------------------------------------------------------------------===//
//// VariableExprAST 
////===----------------------------------------------------------------------===//

void VariableExprAST::toPrint(std::ofstream &File, Traversals T) {
  File << Name;
}

//===----------------------------------------------------------------------===//
//// UnaryExprAST 
////===----------------------------------------------------------------------===//

void UnaryExprAST::toPrint(std::ofstream &File, Traversals T) {
  File << Op; 
  Operand->toPrint(File, T);
}

//===----------------------------------------------------------------------===//
//// BinaryExprAST 
////===----------------------------------------------------------------------===//

void BinaryExprAST::toPrint(std::ofstream &File, Traversals T) {
  LHS->toPrint(File, T);
  File << Op;
  RHS->toPrint(File, T);
}

//===----------------------------------------------------------------------===//
//// IfExprAST
////===----------------------------------------------------------------------===//

void IfExprAST::toPrint(std::ofstream &File, Traversals T) {
  File << " if ";
  Cond->toPrint(File, T);
  File << " then ";
  Then->toPrint(File, T);
  if(Else != nullptr) {
    File << " else ";
    Else->toPrint(File, T);
  }
}

//===----------------------------------------------------------------------===//
//// AssignAST
////===----------------------------------------------------------------------===//

void AssignAST::toPrint(std::ofstream &File, Traversals T) {
  File  << Op;
  Expr->toPrint(File, T);
}

//===----------------------------------------------------------------------===//
//// BlockAST
////===----------------------------------------------------------------------===//

void BlockAST::toPrint(std::ofstream &File, Traversals T) {
  File << "Block (";
  for(int i = 0; i < Exps.size(); i++) {
    Exps[i]->toPrint(File, T);
  }
  File << ")";
}

void BlockAST::addExprAST(std::unique_ptr<ExprAST> Exp) {
  Exps.push_back(std::move(Exp));
}

//===----------------------------------------------------------------------===//
//// PrototypeAST
////===----------------------------------------------------------------------===//

void PrototypeAST::toPrint(std::ofstream &File) {
  File << "\t- Name: " << Name;
}

//===----------------------------------------------------------------------===//
//// ProcedureAST
////===----------------------------------------------------------------------===//

void ProcedureAST::toPrint(std::ofstream &File, Traversals T) {
  File << "\t\t\t-------\n";
  File << "  -> ProcedureAST\n";
  //Proto->toPrint(File);
  //File << std::endl << "\t- ";
  //Block->toPrint(File, T); 
  //File << std::endl;
  File << "\t\t\t-------\n";
}

llvm::Value* ProcedureAST::codegen(llvm::LLVMContext &TheContext) {
  std::string arg = "teste";
  
  return &arg;
}

//llvm::GlobalVariable* DecVarAST::codegen(llvm::LLVMContext &TheContext) {
//  llvm::GlobalVariable::get(TheContext, llvm::Type::IntegerType, );
//  return nullptr;
//}
