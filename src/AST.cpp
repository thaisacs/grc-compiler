#include "AST.hpp"

#include "llvm/IR/Type.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"

#include "Scope.hpp"
#include "Log.hpp"

using namespace grc;

extern llvm::LLVMContext TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
extern std::unique_ptr<Log> LOG;
extern std::shared_ptr<Scope> S;

llvm::IRBuilder<> Builder(TheContext);

//===------------------------------------------------------------------------===//
//// NumberExprAST 
////===----------------------------------------------------------------------===//

void NumberExprAST::toPrint(std::ofstream &File) {
  File << Val;
};

llvm::Value* NumberExprAST::codegen() {
  return llvm::ConstantInt::get(TheContext, llvm::APInt(32, Val));
}

//===------------------------------------------------------------------------===//
//// BooleanExprAST 
////===----------------------------------------------------------------------===//

void BooleanExprAST::toPrint(std::ofstream &File) {
  if(Bool)
    File << "True";
  else
    File << "False";
}

llvm::Value* BooleanExprAST::codegen() {}

//===------------------------------------------------------------------------===//
//// VariableExprAST 
////===----------------------------------------------------------------------===//

void VariableExprAST::toPrint(std::ofstream &File) {
  File << Name;
}

llvm::Value* VariableExprAST::codegen() {
  return S->getVariableValue(Name);
}

//===------------------------------------------------------------------------===//
//// UnaryExprAST 
////===----------------------------------------------------------------------===//

void UnaryExprAST::toPrint(std::ofstream &File) {
  File << Op; 
  Operand->toPrint(File);
}

llvm::Value* UnaryExprAST::codegen() {}

//===------------------------------------------------------------------------===//
//// BinaryExprAST 
////===----------------------------------------------------------------------===//

void BinaryExprAST::toPrint(std::ofstream &File) {
  LHS->toPrint(File);
  File << Op;
  RHS->toPrint(File);
}

llvm::Value* BinaryExprAST::codegen() {
  llvm::Value *L = LHS->codegen();
  llvm::Value *R = RHS->codegen();
  if("+"){
    return Builder.CreateFAdd(L, R, "addtmp");
  }else if("-") {
    return Builder.CreateFSub(L, R, "subtmp");
  }else if("*") {
    return Builder.CreateFMul(L, R, "multmp");
  }else if("<") {
    L = Builder.CreateFCmpULT(L, R, "cmptmp");
    return Builder.CreateUIToFP(L, llvm::Type::getDoubleTy(TheContext), "booltmp");
  }
  return nullptr;
}

//===------------------------------------------------------------------------===//
//// IfExprAST
////===----------------------------------------------------------------------===//

void IfExprAST::toPrint(std::ofstream &File) {
  File << " if ";
  Cond->toPrint(File);
  File << " then ";
  Then->toPrint(File);
  if(Else != nullptr) {
    File << " else ";
    Else->toPrint(File);
  }
}

llvm::Value* IfExprAST::codegen() {
  return Cond->codegen();
}

//===------------------------------------------------------------------------===//
//// WhileExprAST 
////===----------------------------------------------------------------------===//

void WhileExprAST::toPrint(std::ofstream &File) {
  File << " while ";
  if(Cond)
    Cond->toPrint(File);
  else
    std::cout << "null\n";
  File << " block ";
  if(Block)
    Block->toPrint(File);
  else
    std::cout << "null\n";
}

llvm::Value* WhileExprAST::codegen() {}

//===------------------------------------------------------------------------===//
//// AssignAST
////===----------------------------------------------------------------------===//

void AssignAST::toPrint(std::ofstream &File) {
  File  << Op;
  Expr->toPrint(File);
}

llvm::Value* AssignAST::codegen() {
  return Expr->codegen();
}

//===------------------------------------------------------------------------===//
//// VarExprAST
////===----------------------------------------------------------------------===//

void Variable::toPrint(std::ofstream &File) {
  File << Name << " " << isArray << " ";
  if(Expr)
    Expr->toPrint(File);
}

void VarExprAST::addVar(std::unique_ptr<Variable> Var) {
  Vars.push_back(std::move(Var));
}

void VarExprAST::setType(std::unique_ptr<Type> PT) {
  PrimitiveType = std::move(PT);
}

void VarExprAST::toPrint(std::ofstream &File) {
  PrimitiveType->toPrint(File);
  File << " ";
  for(int i = 0; i < Vars.size(); i++) 
    Vars[i]->toPrint(File);
}

llvm::Value* VarExprAST::codegen() {
  //return Expr->codegen();
}

//===------------------------------------------------------------------------===//
//// BlockAST
////===----------------------------------------------------------------------===//

void BlockAST::addExprAST(std::unique_ptr<ExprAST> Exp) {
  Exps.push_back(std::move(Exp));
}

void BlockAST::toPrint(std::ofstream &File) {
  File << "Block (";
  for(int i = 0; i < Exps.size(); i++) {
    Exps[i]->toPrint(File);
  }
  File << ")";
}

llvm::Value* BlockAST::codegen() {
  return nullptr; 
}

//===------------------------------------------------------------------------===//
//// PrototypeAST
////===----------------------------------------------------------------------===//

void PrototypeAST::toPrint(std::ofstream &File) {
  File << "\tName: " << Name;
}

llvm::Function* PrototypeAST::codegen() {
  //std::vector<llvm::Type*> Integers(Args.size(), 
  //    llvm::Type::getInt32Ty(TheContext));
  //llvm::FunctionType *FT = llvm::FunctionType::get(
  //    llvm::Type::getVoidTy(TheContext), Integers, false);
  //llvm::Function* F = llvm::Function::Create(
  //    FT, llvm::Function::ExternalLinkage, Name, TheModule.get());
  
  //unsigned Idx = 0;
  //for(auto &Arg : F->args())
  //  Arg.setName(Args[Idx++]);

  return nullptr;
}

//===------------------------------------------------------------------------===//
//// ProcedureAST
////===----------------------------------------------------------------------===//

void ProcedureAST::toPrint(std::ofstream &File) {
  File << "\t\t\t-------\n";
  File << "  -> ProcedureAST\n";
  Proto->toPrint(File);
  File << std::endl << "\t";
  Body->toPrint(File); 
  File << std::endl;
  File << "\t\t\t-------\n";
}

llvm::Function* ProcedureAST::codegen() {
  //check symbol table
  //llvm::Function *TheFunction = Proto->codegen();

  //if(!TheFunction)
  //  return nullptr;
  
  //llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
  //Builder.SetInsertPoint(BB);

  //for(auto &Arg : TheFunction->args())
  //  S->setVariableValue((std::string)Arg.getName(),(llvm::Value*) &Arg);

  //llvm::Value* RetVal = Body->codegen();
  
  //if (llvm::Value *RetVal = Body->codegen()) {
  //  Builder.CreateRet(RetVal);
  //  verifyFunction(*TheFunction);
  //  return TheFunction;
  //}

  //  RetVal->print(llvm::errs());
  //  Builder.CreateRet(RetVal);

  //llvm::VerifyFunction(*TheFunction);

  return nullptr;
}
