#include "AST.hpp"

#include "llvm/IR/Type.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/GlobalVariable.h"

#include "Scope.hpp"
#include "Log.hpp"

using namespace grc;

extern llvm::LLVMContext TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
extern std::unique_ptr<Log> LOG;
extern std::shared_ptr<Scope> S;

llvm::IRBuilder<> Builder(TheContext);

//===------------------------------------------------------------------------===//
//// IntegersExprAST 
////===----------------------------------------------------------------------===//

static std::map<std::string, llvm::AllocaInst*> NamedValues;

//===------------------------------------------------------------------------===//
//// IntegersExprAST 
////===----------------------------------------------------------------------===//

void IntegersExprAST::toPrint(std::ofstream &File) {
  File << "{ ";
  for(int i = 0; i < Ints.size(); i++) {
    if(i < Ints.size() - 1)
      File << Ints[i] << ", ";
    else
      File << Ints[i] << " ";
  }
  File << "}";
};

llvm::Value* IntegersExprAST::codegen() {
}

//===------------------------------------------------------------------------===//
//// BooleansExprAST 
////===----------------------------------------------------------------------===//

void BooleansExprAST::toPrint(std::ofstream &File) {
  File << "{ ";
  for(int i = 0; i < Bools.size(); i++) {
    if(i < Bools.size() - 1)
      File << Bools[i] << ", ";
    else
      File << Bools[i] << " ";
  }
  File << "}";
};

llvm::Value* BooleansExprAST::codegen() {
}

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
//// StringExprAST 
////===----------------------------------------------------------------------===//

void StringExprAST::toPrint(std::ofstream &File) {
  File << String;
}

llvm::Value* StringExprAST::codegen() {}

//===------------------------------------------------------------------------===//
//// VariableExprAST 
////===----------------------------------------------------------------------===//

void VariableExprAST::toPrint(std::ofstream &File) {
  File << Name;
}

llvm::Value* VariableExprAST::codegen() {
  auto Symb = S->find(Name);
  //return S->getValue();
  return nullptr;
}

//===------------------------------------------------------------------------===//
//// UnaryExprAST 
////===----------------------------------------------------------------------===//

void UnaryExprAST::toPrint(std::ofstream &File) {
  File << Op; 
  Operand->toPrint(File);
}

llvm::Value* UnaryExprAST::codegen() {
  return nullptr;
}

//===------------------------------------------------------------------------===//
//// BinaryExprAST 
////===----------------------------------------------------------------------===//

void BinaryExprAST::toPrint(std::ofstream &File) {
  File << Op;
  LHS->toPrint(File);
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
  //File << " if ";
  //Cond->toPrint(File);
  //File << " then ";
  //Then->toPrint(File);
  //if(Else != nullptr) {
  //  File << " else ";
  //  Else->toPrint(File);
  //}
}

llvm::Value* IfExprAST::codegen() {
  return Cond->codegen();
}

//===------------------------------------------------------------------------===//
//// WhileExprAST 
////===----------------------------------------------------------------------===//

void WhileExprAST::toPrint(std::ofstream &File) {
  //File << " while ";
  //if(Cond)
  //  Cond->toPrint(File);
  //else
  //  std::cout << "null\n";
  //File << " block ";
  //if(Block)
  //  Block->toPrint(File);
  //else
  //  std::cout << "null\n";
}

llvm::Value* WhileExprAST::codegen() {}

//===------------------------------------------------------------------------===//
//// AssignAST
////===----------------------------------------------------------------------===//

void AssignAST::toPrint(std::ofstream &File) {
  //File  << Op;
  //Expr->toPrint(File);
}

llvm::Value* AssignAST::codegen() {
  return Expr->codegen();
}

//===------------------------------------------------------------------------===//
//// VarExprAST
////===----------------------------------------------------------------------===//

void Var::toPrint(std::ofstream &File) {
  File << "    " << Name << " ";
  if(Expr)
    Expr->toPrint(File);
}

llvm::Value* VarExprAST::codegen() {
}

void VarExprAST::toPrint(std::ofstream &File) {
  File << " -> new VarExprAST\n";
  for(int i = 0; i < Vars.size(); i++) {
    Vars[i]->toPrint(File);
    File << "\n";
  }
}

//===------------------------------------------------------------------------===//
//// WriteExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* WriteExprAST::codegen() {
  llvm::Function* putsFunc = TheModule->getFunction("printf");
  std::vector<llvm::Value *> ArgsV;
  llvm::Value *formatStr = Builder.CreateGlobalStringPtr("amazing %d\n");
  ArgsV.push_back(formatStr);
  ArgsV.push_back(llvm::ConstantInt::get(TheContext, llvm::APInt(32, 5)));
  Builder.CreateCall(putsFunc, ArgsV, "hello");
  return nullptr;
}

void WriteExprAST::toPrint(std::ofstream &File) {
}

//===------------------------------------------------------------------------===//
//// CallExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* CallExprAST::codegen() {
  std::vector<llvm::Value *> ArgsV;
  llvm::Function *CalleeF = TheModule->getFunction(Callee);
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
      ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
              return nullptr;
        }
  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");  
}

void CallExprAST::toPrint(std::ofstream &File) {
  //PrimitiveType->toPrint(File);
  //File << " ";
  //for(int i = 0; i < Vars.size(); i++) 
  //  Vars[i]->toPrint(File);
}

//===------------------------------------------------------------------------===//
//// BlockExprAST
////===----------------------------------------------------------------------===//

void BlockExprAST::toPrint(std::ofstream &File) {
  //File << "Block (";
  //for(int i = 0; i < Exps.size(); i++) {
  //  Exps[i]->toPrint(File);
  //}
  //File << ")";
}

llvm::Value* BlockExprAST::codegen() {
  for(int i = 0; i < Exps.size(); i++) {
    Exps[i]->codegen();
  }
  return nullptr; 
}

//===------------------------------------------------------------------------===//
//// PrototypeAST
////===----------------------------------------------------------------------===//

void PrototypeAST::toPrint(std::ofstream &File) {
  File << "\tName: " << Name;
}

llvm::Function* PrototypeAST::codegen() {
  std::vector<llvm::Type*> ArgsVector;
  llvm::FunctionType *FT = nullptr;
  /*
  for(int i = Args.size()-1; i >= 0; i--) {
    auto Var = S->find(Args[i]);  
    auto T = Var->getType();
    //set args type
    
    switch(T->getPrimitiveType()->getBasicType()) {
      case BasicType::Int:
        if(T->getIsArray()) {
          ArgsVector.push_back(llvm::Type::getInt32PtrTy(TheContext));
        }else {
          ArgsVector.push_back(llvm::Type::getInt32Ty(TheContext));
        }
        break;
      case BasicType::Bool:
        if(T->getIsArray()) {
          ArgsVector.push_back(llvm::Type::getInt8PtrTy(TheContext));
        }else {
          ArgsVector.push_back(llvm::Type::getInt8Ty(TheContext));
        }
        break;
      case BasicType::String:
        ArgsVector.push_back(llvm::Type::getInt8PtrTy(TheContext));
        break;
      case BasicType::Void:
        ArgsVector.push_back(llvm::Type::getVoidTy(TheContext));
        break;
      default: // BasicType::Undefined:
        ArgsVector.push_back(llvm::Type::getVoidTy(TheContext));
    }
  }
  
  auto Func = S->find(Name);
  auto T = Func->getType();
  switch(T->getPrimitiveType()->getBasicType()) {
    case BasicType::Int:
      FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(TheContext), ArgsVector, false);
      break;
    case BasicType::Void:
      FT = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), ArgsVector, false);
      break;
  }
  
  llvm::Function* F = llvm::Function::Create(
      FT, llvm::Function::ExternalLinkage, Name, TheModule.get());
  
  unsigned Idx = 0;
  for(auto &Arg : F->args())
    Arg.setName(Args[Args.size() - 1 - Idx++]);
*/
  return nullptr;
}

//===------------------------------------------------------------------------===//
//// SubroutineAST
////===----------------------------------------------------------------------===//

void SubroutineAST::toPrint(std::ofstream &File) {
  //File << "\t\t\t-------\n";
  //File << "  -> ProcedureAST\n";
  //Proto->toPrint(File);
  //File << std::endl << "\t";
  //Body->toPrint(File); 
  //File << std::endl;
  //File << "\t\t\t-------\n";
}

llvm::Function* SubroutineAST::codegen() {
  //check symbol table
  llvm::Function *TheFunction = Proto->codegen();

  if(!TheFunction)
    return nullptr;
  
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  //for(auto &Arg : TheFunction->args())
  //  S->setVariableValue((std::string)Arg.getName(),(llvm::Value*) &Arg);

  llvm::Value* RetVal = Body->codegen();
  
  //if (llvm::Value *RetVal = Body->codegen()) {
  //  Builder.CreateRet(RetVal);
  //  verifyFunction(*TheFunction);
  //  return TheFunction;
  //}

  //  RetVal->print(llvm::errs());
  Builder.CreateRet(nullptr);

  //llvm::VerifyFunction(*TheFunction);
  
  return TheFunction;
}

//llvm::Type* I = llvm::IntegerType::getInt8Ty(TheContext);
//llvm::ArrayType* arrayType = llvm::ArrayType::get(I, 5);
