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
#include "llvm/ADT/APFloat.h"
#include "llvm/Support/Casting.h"

#include "llvm/IR/GlobalValue.h"

#include "llvm/IR/NoFolder.h"

#include "Scope.hpp"
#include "Log.hpp"
#include "Error.hpp"

using namespace grc;

extern llvm::LLVMContext TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
extern std::shared_ptr<Scope> S;

extern int yylineno;

llvm::IRBuilder<> Builder(TheContext);

//===------------------------------------------------------------------------===//
//// Variable IR
////===----------------------------------------------------------------------===//

//static std::map<std::string, llvm::AllocaInst*> NamedValues;

static llvm::AllocaInst* CreateEntryBlockAlloca(llvm::Function *TheFunction,
    const std::string &VarName) {
  std::shared_ptr<Type> T = S->find(VarName)->getType();
  auto AType = T->getArrayType();
  auto PType = T->getPrimitiveType();
  
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), 
          TheFunction->getEntryBlock().begin());

  switch(PType->getBasicType()) {
    case BasicType::Int:
      if(AType->isArray) {
        llvm::Type* I = llvm::IntegerType::getInt32Ty(TheContext);
        return TmpB.CreateAlloca(llvm::ArrayType::get(I, AType->Size), 0, 
          VarName.c_str());
      }else {
        return TmpB.CreateAlloca(llvm::Type::getInt32Ty(TheContext), 0, 
          VarName.c_str());
      }
      break;
    case BasicType::Bool:
      if(AType->isArray) {
        llvm::Type* I = llvm::IntegerType::getInt1Ty(TheContext);
        return TmpB.CreateAlloca(llvm::ArrayType::get(I, AType->Size), 0, 
          VarName.c_str());
      }else {
        return TmpB.CreateAlloca(llvm::Type::getInt1Ty(TheContext), 0, 
          VarName.c_str());
      }
     break;
    case BasicType::String:
      llvm::Type* I = llvm::IntegerType::getInt8Ty(TheContext);
      return TmpB.CreateAlloca(llvm::ArrayType::get(I, PType->getSize() + 1), 0, 
          VarName.c_str());
      break;
  } 
}

static llvm::AllocaInst* CreateEntryBlockAllocaArgs(llvm::Function *TheFunction,
    const std::string &VarName) {
  std::shared_ptr<Type> T = S->find(VarName)->getType();
  auto AType = T->getArrayType();
  auto PType = T->getPrimitiveType();
  
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), 
          TheFunction->getEntryBlock().begin());

  switch(PType->getBasicType()) {
    case BasicType::Int:
      if(AType->isArray) {
        return TmpB.CreateAlloca(llvm::Type::getInt32PtrTy(TheContext), 0, 
          VarName.c_str());
      }else {
        return TmpB.CreateAlloca(llvm::Type::getInt32Ty(TheContext), 0, 
          VarName.c_str());
      }
      break;
    case BasicType::Bool:
      if(AType->isArray) {
        return TmpB.CreateAlloca(llvm::Type::getInt1PtrTy(TheContext), 0, 
          VarName.c_str());
      }else {
        return TmpB.CreateAlloca(llvm::Type::getInt1Ty(TheContext), 0, 
          VarName.c_str());
      }
     break;
    case BasicType::String:
      return TmpB.CreateAlloca(llvm::Type::getInt8PtrTy(TheContext), 0, 
          VarName.c_str());
      break;
  } 
}

llvm::GlobalVariable* AllocaGlobal(std::string Name) {
  TheModule->getOrInsertGlobal(Name, Builder.getInt32Ty());
  llvm::GlobalVariable *gVar = TheModule->getNamedGlobal(Name);
  gVar->setLinkage(llvm::GlobalValue::CommonLinkage);
  gVar->setAlignment(4);
  return gVar;
}

//===------------------------------------------------------------------------===//
//// IntegersExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* IntegersExprAST::codegen() {}

BasicType IntegersExprAST::getResultingType() {
  return BasicType::IntArray;
}

//===------------------------------------------------------------------------===//
//// BooleansExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* BooleansExprAST::codegen() {}

BasicType BooleansExprAST::getResultingType() {
  return BasicType::BoolArray;
}

//===------------------------------------------------------------------------===//
//// NumberExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* NumberExprAST::codegen() {
  return llvm::ConstantInt::get(TheContext, llvm::APInt(32, Val));
}

BasicType NumberExprAST::getResultingType() {
  return BasicType::Int;
}

//===------------------------------------------------------------------------===//
//// BooleanExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* BooleanExprAST::codegen() {
  if(Bool)
    return llvm::ConstantInt::get(TheContext, llvm::APInt(1, 1));
  else
    return llvm::ConstantInt::get(TheContext, llvm::APInt(1, 0));
}

BasicType BooleanExprAST::getResultingType() {
  return BasicType::Bool;
}

//===------------------------------------------------------------------------===//
//// StringExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* StringExprAST::codegen() {
  std::string Str;
  unsigned i = 0;
  while(i < String.size()) {
    if(String[i] != '\"') {
      Str +=  String[i];
    }    
    i++;
  }
  Str += "\n"; 
  return Builder.CreateGlobalStringPtr(Str);
}

BasicType StringExprAST::getResultingType() {
  return BasicType::String;
}

//===------------------------------------------------------------------------===//
//// VariableExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* VariableExprAST::codegen() {
  std::shared_ptr<Symbol> Symb = S->find(Name);
  auto VarSymbol = static_cast<VariableSymbol*> (Symb.get());
  
  if(!VarSymbol)
   return nullptr; 
 
  auto VarBType = VarSymbol->getType()->getPrimitiveType()->getBasicType();
  
  //llvm::Value *VarValue = NamedValues[Name];
  llvm::Value *VarValue = VarSymbol->getValue();
  
  if (!VarValue)
    return nullptr;

  if(!VarSymbol->getType()->getArrayType()->isArray && VarBType != BasicType::String) {
    // Load the value.
    return Builder.CreateLoad(VarValue, Name.c_str());
  }else {
    if(VarSymbol->getArgument()) {
      switch(VarBType) {
        case BasicType::String:
          return Builder.CreateLoad(VarValue, Name.c_str());
        case BasicType::Int:
        case BasicType::Bool:
          llvm::Value *Element = Builder.CreateLoad(VarValue, Name.c_str());
          llvm::Value *I = Index->codegen();
          llvm::Value *Num = Builder.CreateGEP(Element, I);
          return Builder.CreateLoad(Num, Name.c_str());
      }
    }else {
      llvm::Type *Ty, *TA;
      llvm::Value *Num, *I, *PtrArray;
      switch(VarBType) {
        case BasicType::String:
          Ty = llvm::IntegerType::getInt8Ty(TheContext);
          TA = llvm::ArrayType::get(Ty, 
              VarSymbol->getType()->getPrimitiveType()->getSize() + 1);
          return Builder.CreateConstGEP2_32(TA, VarValue, 0, 0);
        case BasicType::Int:
          Ty = llvm::IntegerType::getInt32Ty(TheContext);
          TA = llvm::ArrayType::get(Ty, VarSymbol->getType()->getArrayType()->Size);
          if(!Index) {
            return Builder.CreateConstGEP2_32(TA, VarValue, 0, 0);
          }else {
            I = Index->codegen();
            PtrArray = Builder.CreateConstGEP2_32(TA, VarValue, 0, 0);
            Num = Builder.CreateGEP(PtrArray, I);
            return Builder.CreateLoad(Num, Name.c_str());
          }
        case BasicType::Bool:
          Ty = llvm::IntegerType::getInt1Ty(TheContext);
          TA = llvm::ArrayType::get(Ty, VarSymbol->getType()->getArrayType()->Size);
          if(!Index) {
            return Builder.CreateConstGEP2_32(TA, VarValue, 0, 0);
          }else {
            I = Index->codegen();
            PtrArray = Builder.CreateConstGEP2_32(TA, VarValue, 0, 0);
            Num = Builder.CreateGEP(PtrArray, I);
            return Builder.CreateLoad(Num, Name.c_str());
          }
      }
    }
  }
}

llvm::Value* VariableExprAST::getAllocaCodegen() {
  std::shared_ptr<Symbol> Symb = S->find(Name);
  auto VarSymbol = static_cast<VariableSymbol*> (Symb.get());
  
  if(!VarSymbol)
   return nullptr; 
 
  auto VarBType = VarSymbol->getType()->getPrimitiveType()->getBasicType();
  
  llvm::Value *VarValue = VarSymbol->getValue();
  //llvm::Value *VarValue = NamedValues[Name];
  
  if (!VarValue)
    return nullptr;

  if(!VarSymbol->getType()->getArrayType()->isArray && VarBType != BasicType::String) {
    // Load the value.
    return VarValue;
  }else {
    if(VarSymbol->getArgument()) {
      switch(VarBType) {
        case BasicType::Int:
        case BasicType::Bool:
          llvm::Value *Element = Builder.CreateLoad(VarValue, Name.c_str());
          llvm::Value *I = Index->codegen();
          return Builder.CreateGEP(Element, I);
      }
    }else {
      llvm::Type *Ty, *TA;
      llvm::Value *Num, *I, *PtrArray;
      switch(VarBType) {
        case BasicType::Int:
          Ty = llvm::IntegerType::getInt32Ty(TheContext);
          TA = llvm::ArrayType::get(Ty, VarSymbol->getType()->getArrayType()->Size);
          I = Index->codegen();
          PtrArray = Builder.CreateConstGEP2_32(TA, VarValue, 0, 0);
          return Builder.CreateGEP(PtrArray, I);
        case BasicType::Bool:
          Ty = llvm::IntegerType::getInt1Ty(TheContext);
          TA = llvm::ArrayType::get(Ty, VarSymbol->getType()->getArrayType()->Size);
          I = Index->codegen();
          PtrArray = Builder.CreateConstGEP2_32(TA, VarValue, 0, 0);
          return Builder.CreateGEP(PtrArray, I);
      }
    }
  }
}

BasicType VariableExprAST::getResultingType() {
  std::shared_ptr<Symbol> VarSymbol = S->find(Name);
  if(VarSymbol) {
    switch(VarSymbol->getType()->getPrimitiveType()->getBasicType()) {
      case BasicType::Int:
        if(VarSymbol->getType()->getArrayType()->isArray) {
          if(!Index) {
            return BasicType::IntArray;
          }else {
            return BasicType::Int;
          }
        }else {
          return BasicType::Int;
        }
      case BasicType::Bool:
        if(VarSymbol->getType()->getArrayType()->isArray) {
          if(!Index) {
            return BasicType::BoolArray;
          }else {
            return BasicType::Bool;
          }
        }else {
          return BasicType::Bool;
        }
      case BasicType::Void:
        return BasicType::Void;
      case BasicType::Undefined:
        return BasicType::Undefined;
      case BasicType::String:
        return BasicType::String;
    }
  }

  std::string MsgError = "variable '" + Name + "' was not declared";
  LogError(MsgError, yylineno);
  
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// UnaryExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* UnaryExprAST::codegen() {
  llvm::Value *R = Operand->codegen();
  if(Op == "!") {
    return Builder.CreateNot(R, "tmpNot");
  }else {
    return Builder.CreateNeg(R, "tmpNeg");
  }
  return nullptr;
}

BasicType UnaryExprAST::getResultingType() {
  auto BTOperand = Operand->getResultingType();
  if(Op == "!") {
    if(!(BTOperand == BasicType::Bool)) {
      return BasicType::Undefined;
    }
    return BasicType::Bool;
  }else if(Op == "-") {
    if(!(BTOperand == BasicType::Int)) {
      return BasicType::Undefined;
    }
    return BasicType::Int;
  }
  return BasicType::Int;
}

//===------------------------------------------------------------------------===//
//// BinaryExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* BinaryExprAST::codegen() {
  llvm::Value *L = LHS->codegen();
  llvm::Value *R = RHS->codegen();

  if(!L || !R)
    return nullptr;

  if(Op == "+"){
    return Builder.CreateAdd(L, R, "resultmp");
  }else if(Op == "-") {
    return Builder.CreateSub(L, R, "resultmp");
  }else if(Op == "*") {
    return Builder.CreateMul(L, R, "resultmp");
  }else if(Op == "/") {
    return Builder.CreateSDiv(L, R, "resultmp");
  }else if(Op == "%") {
    llvm::Value *RD = Builder.CreateSDiv(L, R, "resultmp");
    llvm::Value *Result1 = Builder.CreateMul(RD, R, "resultmp");
    return Builder.CreateSub(L, Result1, "resultmp");
  }else if(Op == "<") {
    return Builder.CreateICmpSLT(L, R, "resultmp");
  }else if(Op == ">") {
    return Builder.CreateICmpSGT(L, R, "cmptmp");
  }else if(Op == ">=") {
    return Builder.CreateICmpSGE(L, R, "cmptmp");
  }else if(Op == "<=") {
    return Builder.CreateICmpSLE(L, R, "cmptmp");
  }else if(Op == "!=") {
    return Builder.CreateICmpNE(L, R, "cmptmp");
  }else if(Op == "==") {
    return Builder.CreateICmpEQ(L, R, "cmptmp");
  }else if(Op == "&&") {
    return Builder.CreateAnd(L, R, "andtmp");
  }else if(Op == "||") {
    return Builder.CreateOr(L, R, "ortmp");
  }

  return nullptr;
}

BasicType BinaryExprAST::getResultingType() {
  auto BTL = LHS->getResultingType();
  auto BTR = RHS->getResultingType();

  if(Op == "+" || Op == "-" || Op == "*" || Op == "/" || Op == "%"){
    if(!(BTL == BasicType::Int) || !(BTR == BasicType::Int)) {
      return BasicType::Undefined;
    }
    return BasicType::Int;
  }else if(Op == "<" || Op == ">" || Op == ">=" || Op == "<=" 
      || Op == "!=" || Op == "==") {
    if(!(BTL == BasicType::Int) || !(BTR == BasicType::Int)) {
      return BasicType::Undefined;
    }
    return BasicType::Bool;
  }else if(Op == "&&" || Op == "||") {
    if(!(BTL == BasicType::Bool) || !(BTR == BasicType::Bool)) {
      return BasicType::Undefined;
    }
    return BasicType::Bool;
  }
}

//===------------------------------------------------------------------------===//
//// TernaryExprAST
////===----------------------------------------------------------------------===//

llvm::Value* TernaryExprAST::codegen() {}

BasicType TernaryExprAST::getResultingType() {}

//===------------------------------------------------------------------------===//
//// IfExprAST
////===----------------------------------------------------------------------===//

llvm::Value* IfExprAST::codegen() {
  llvm::Value *CondV = Cond->codegen();
  if(!CondV)
    return nullptr;

  llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(TheContext, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(TheContext, "ifcont");

  Builder.CreateCondBr(CondV, ThenBB, ElseBB);

  Builder.SetInsertPoint(ThenBB);

  Then->codegen();
  if (!Then){
    return nullptr;
  }

  Builder.CreateBr(MergeBB);

  ThenBB = Builder.GetInsertBlock();

  TheFunction->getBasicBlockList().push_back(ElseBB);
  Builder.SetInsertPoint(ElseBB);

  if(Else) {
    Else->codegen();
  }

  Builder.CreateBr(MergeBB);
  ElseBB = Builder.GetInsertBlock();

  TheFunction->getBasicBlockList().push_back(MergeBB);
  Builder.SetInsertPoint(MergeBB);
}

BasicType IfExprAST::getResultingType() {
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// ForExprAST
////===----------------------------------------------------------------------===//

llvm::Value* ForExprAST::codegen() {
  llvm::Value *StartVal = Start->codegen();
  if (!StartVal)
    return nullptr;

  llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

  llvm::BasicBlock *CondBB =
    llvm::BasicBlock::Create(TheContext, "condloop", TheFunction);
  llvm::BasicBlock *LoopBB =
    llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
  llvm::BasicBlock *AfterBB =
    llvm::BasicBlock::Create(TheContext, "afterloop", TheFunction);

  Builder.CreateBr(CondBB);

  Builder.SetInsertPoint(CondBB); 
  llvm::Value *EndCond = End->codegen();
  if (!EndCond)
    return nullptr;
  Builder.CreateCondBr(EndCond, LoopBB, AfterBB);

  Builder.SetInsertPoint(LoopBB);
  if(!Body->codegen())
    return nullptr;
  llvm::Value *StepVal = Step->codegen();
  if(!StepVal)
    return nullptr;
  Builder.CreateBr(CondBB);

  Builder.SetInsertPoint(AfterBB);
}

BasicType ForExprAST::getResultingType() {
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// WhileExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* WhileExprAST::codegen() {
  llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

  llvm::BasicBlock *CondBB = 
    llvm::BasicBlock::Create(TheContext, "condloop", TheFunction); 
  llvm::BasicBlock *LoopBB = 
    llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
  llvm::BasicBlock *AfterBB = 
    llvm::BasicBlock::Create(TheContext, "afterloop", TheFunction);

  Builder.CreateBr(CondBB);

  Builder.SetInsertPoint(CondBB);
  
  llvm::Value *CondV = Cond->codegen();
  if(!Cond)
    return nullptr;

  Builder.CreateCondBr(CondV, LoopBB, AfterBB);

  Builder.SetInsertPoint(LoopBB);
  if(!Block->codegen())
    return nullptr;
  Builder.CreateBr(CondBB);

  Builder.SetInsertPoint(AfterBB);
}

BasicType WhileExprAST::getResultingType() {
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// AssignExprAST
////===----------------------------------------------------------------------===//

llvm::Value* AssignExprAST::codegen() {
  llvm::Value *ExprVal = Expr->codegen(); 
  
  if(!ExprVal)
    return nullptr;

  llvm::Value *VarValue = Var->codegen();
  if (!VarValue)
    return nullptr;
    
  llvm::Value *Result;

  if(Op == "=") {
    Result = ExprVal;
  }else if(Op == "+=") {
    Result = Builder.CreateAdd(VarValue, ExprVal, "resultmp");
  }else if(Op == "-=") {
    Result = Builder.CreateSub(VarValue, ExprVal, "resultmp");
  }else if(Op == "*=") {
    Result = Builder.CreateMul(VarValue, ExprVal, "resultmp");
  }else if(Op == "/=") {
    llvm::Value *Result = Builder.CreateSDiv(VarValue, ExprVal, "resultmp");
  }else if(Op == "%=") {
    llvm::Value *RD = Builder.CreateSDiv(VarValue, ExprVal, "resultmp");
    llvm::Value *Result1 = Builder.CreateMul(RD, ExprVal, "resultmp");
    Result1 = Builder.CreateSub(VarValue, Result1, "resultmp");
  } 
  
  /*
  std::shared_ptr<Symbol> Symb = S->find(Var->getName());
  auto VarSymbol = static_cast<VariableSymbol*> (Symb.get());
  
  if(!VarSymbol)
    return nullptr; 
 
  auto VarBType = VarSymbol->getType()->getPrimitiveType()->getBasicType();
  
  llvm::Value *Variable = NamedValues[Var->getName()];
  
  if(!Variable)
    return nullptr;

  if(!VarSymbol->getType()->getArrayType()->isArray) {
    Builder.CreateStore(Result, Variable);
  }else {
    if(VarSymbol->getArgument()) {
      llvm::Value *Element = Builder.CreateLoad(Variable, Var->getName().c_str());
      llvm::Value *I = Var->getIndex();
      llvm::Value *V = Builder.CreateGEP(Element, I);
      Builder.CreateStore(Result, V);
    }else {
      llvm::Value *I = Var->getIndex();
      llvm::Type *Ty = llvm::IntegerType::getInt32Ty(TheContext);
      llvm::Type *TA = llvm::ArrayType::get(Ty, VarSymbol->getType()->getArrayType()->Size);
      llvm::Value *PtrArray = Builder.CreateConstGEP2_32(TA, Variable, 0, 0);
      llvm::Value *Num = Builder.CreateGEP(PtrArray, I);
      Builder.CreateStore(Result, Num);
    }
  }*/

  llvm::Value *AllocaVar = Var->getAllocaCodegen();

  Builder.CreateStore(Result, AllocaVar);

  return Result;
}

BasicType AssignExprAST::getResultingType() {
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// VarExprAST
////===----------------------------------------------------------------------===//

llvm::Value* VarExprAST::globalCodegen() {
  //llvm::Type* I = llvm::IntegerType::getInt32Ty(TheContext);
  //llvm::GlobalVariable(*TheModule.get(), I, false, 
  //    llvm::GlobalValue::CommonLinkage, nullptr, "a");

  //for(unsigned i = 0; i < Vars.size(); i++) {
  //}
  llvm::GlobalVariable *gVar = AllocaGlobal("a");
}

llvm::Value* VarExprAST::codegen() {
  llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

  if(!TheFunction)
    return nullptr;

  for(unsigned i = 0, e = Vars.size(); i != e; ++i) {
    const std::string &VarName = Vars[i]->getVar()->getName();
    llvm::Value *InitVal;

    std::shared_ptr<Type> T = S->find(VarName)->getType();
    auto AType = T->getArrayType();
    auto PType = T->getPrimitiveType();
    
    if(AType->isArray) {
      ExprAST *Init = Vars[i]->getExpr();
      switch(PType->getBasicType()) {
        case BasicType::Int:
          if(!Init) {
            llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
            llvm::Type* I = llvm::IntegerType::getInt32Ty(TheContext);
            llvm::Type* TA = llvm::ArrayType::get(I, AType->Size);

            llvm::Value *PtrArray = Builder.CreateConstGEP2_32(TA, Alloca, 0, 0);

            for(unsigned j = 0; j < AType->Size; j++) {
              llvm::Value *Base = Builder.CreateGEP(PtrArray, Builder.getInt32(j));
              Builder.CreateStore(Builder.getInt32(0), Base); 
            }

            std::shared_ptr<Symbol> Symb = S->find(VarName);
            auto VarSymbol = static_cast<VariableSymbol*>(Symb.get());
            VarSymbol->setValue(Alloca);
            
            //NamedValues[VarName] = Alloca;
          }else {
            llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
            llvm::Type* I = llvm::IntegerType::getInt32Ty(TheContext);
            llvm::Type* TA = llvm::ArrayType::get(I, AType->Size);
            IntegersExprAST *IEAST = static_cast<IntegersExprAST*>(Init);
            std::vector<int> Ints = IEAST->getInts();
            llvm::Value *PtrArray = Builder.CreateConstGEP2_32(TA, Alloca, 0, 0);

            for(unsigned j = 0; j < Ints.size(); j++) {
              llvm::Value *Base = Builder.CreateGEP(PtrArray, Builder.getInt32(j));
              Builder.CreateStore(Builder.getInt32(Ints[j]), Base); 
            }

            std::shared_ptr<Symbol> Symb = S->find(VarName);
            auto VarSymbol = static_cast<VariableSymbol*>(Symb.get());
            VarSymbol->setValue(Alloca);

            //NamedValues[VarName] = Alloca;
          }
          break;
        case BasicType::Bool:
          if(!Init) {
            llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
            llvm::Type* I = llvm::IntegerType::getInt1Ty(TheContext);
            llvm::Type* TA = llvm::ArrayType::get(I, AType->Size);

            llvm::Value *PtrArray = Builder.CreateConstGEP2_32(TA, Alloca, 0, 0);

            for(unsigned j = 0; j < AType->Size; j++) {
              llvm::Value *Base = Builder.CreateGEP(PtrArray, Builder.getInt32(j));
              Builder.CreateStore(Builder.getInt1(0), Base); 
            }

            std::shared_ptr<Symbol> Symb = S->find(VarName);
            auto VarSymbol = static_cast<VariableSymbol*>(Symb.get());
            VarSymbol->setValue(Alloca);
            
            //NamedValues[VarName] = Alloca;
          }else {
            llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
            BooleansExprAST *IEAST = static_cast<BooleansExprAST*>(Init);
            std::vector<bool> Bools = IEAST->getBools();

            llvm::Type* I = llvm::IntegerType::getInt1Ty(TheContext);
            llvm::Type* TA = llvm::ArrayType::get(I, AType->Size);

            llvm::Value *PtrArray = Builder.CreateConstGEP2_32(TA, Alloca, 0, 0);
            for(unsigned j = 0; j < Bools.size(); j++) {
              llvm::Value *Base = Builder.CreateGEP(PtrArray, Builder.getInt32(j));
              Builder.CreateStore(Builder.getInt1(Bools[j]), Base); 
            }
            
            std::shared_ptr<Symbol> Symb = S->find(VarName);
            auto VarSymbol = static_cast<VariableSymbol*>(Symb.get());
            VarSymbol->setValue(Alloca);
            
            //NamedValues[VarName] = Alloca;
          }
          break;
      }
    }else if(PType->getBasicType() == BasicType::String) {
      llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
      ExprAST *Init = Vars[i]->getExpr();
      llvm::Type* I = llvm::IntegerType::getInt8Ty(TheContext);
      llvm::Type* TA = llvm::ArrayType::get(I, PType->getSize() + 1);
      
      llvm::Value *PtrArray = Builder.CreateConstGEP2_32(TA, Alloca, 0, 0);
      
      if(!Init) {
        for(unsigned j = 0; j <= PType->getSize(); j++) {
          llvm::Value *Base = Builder.CreateGEP(PtrArray, Builder.getInt32(j));
          Builder.CreateStore(Builder.getInt8(0), Base); 
        }
      }else {
        StringExprAST *StringInit = static_cast<StringExprAST*>(Init);
        std::string S = StringInit->getString();
        unsigned j;
        for(unsigned j = 1; j < S.size()-1; j++) {
          int c = (int)S[j];
          llvm::Value *Base = Builder.CreateGEP(PtrArray, Builder.getInt32(j-1));
          Builder.CreateStore(Builder.getInt8(c), Base); 
        }
        llvm::Value *Base = Builder.CreateGEP(PtrArray, Builder.getInt32(j-1));
        Builder.CreateStore(Builder.getInt8(0), Base); 
      }
      
      std::shared_ptr<Symbol> Symb = S->find(VarName);
      auto VarSymbol = static_cast<VariableSymbol*>(Symb.get());
      VarSymbol->setValue(Alloca);
      
      //NamedValues[VarName] = Alloca;
    }else {
      ExprAST *Init = Vars[i]->getExpr();
      if(Init) {
        InitVal = Init->codegen();
        if(!InitVal)
          return nullptr;
      }else { // If not specified, use 0.
        switch(PType->getBasicType()) {
          case BasicType::Int:
            InitVal = llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
            break;
          case BasicType::Bool:
            InitVal =  llvm::ConstantInt::get(TheContext, llvm::APInt(8, 0));
        }
      }
      llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
      Builder.CreateStore(InitVal, Alloca);
      
      std::shared_ptr<Symbol> Symb = S->find(VarName);
      auto VarSymbol = static_cast<VariableSymbol*>(Symb.get());
      VarSymbol->setValue(Alloca);
      //NamedValues[VarName] = Alloca;
    }
  } 
}

BasicType VarExprAST::getResultingType() {
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// WriteExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* WriteExprAST::codegen() {
  llvm::Function* putsFunc = TheModule->getFunction("printf");
  if(putsFunc) {
    std::vector<llvm::Value*> ArgsV;
    for(unsigned i = 0; i < Args.size(); i++) {
      ArgsV.push_back(Args[i]->codegen());
    }
    Builder.CreateCall(putsFunc, ArgsV, "retWrite");
  }else {
    std::string ErrorMsg = "error[all]: 'write' function was not defined. You must import io library. Use 'import io'.";
    LogError(ErrorMsg);
    return nullptr;
  }
}

BasicType WriteExprAST::getResultingType() {
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// ReadExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* ReadExprAST::codegen() {
  std::vector<llvm::Value*> ArgsV;
  llvm::Function* readFunc = TheModule->getFunction("scanf");
  llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
  if(readFunc) {
    // Load the value.
    llvm::Value *formatStr = Builder.CreateGlobalStringPtr("%d");
    ArgsV.push_back(formatStr);
  
    std::shared_ptr<Symbol> Symb = S->find(Name);
    auto VarSymbol = static_cast<VariableSymbol*> (Symb.get());
  
    if(!VarSymbol)
      return nullptr; 
 
    auto VarBType = VarSymbol->getType()->getPrimitiveType()->getBasicType();
  
    //llvm::Value *VarValue = NamedValues[Name];
    llvm::Value *VarValue = VarSymbol->getValue();

  
    if (!VarValue)
      return nullptr;

    if(!VarSymbol->getType()->getArrayType()->isArray) {
      ArgsV.push_back(VarValue);
    }else {
      if(VarSymbol->getArgument()) {
        llvm::Value *Element = Builder.CreateLoad(VarValue, Name.c_str());
        llvm::Value *I = Index->codegen();
        llvm::Value *Num = Builder.CreateGEP(Element, I);
        ArgsV.push_back(Num);
      }else {
        llvm::Value *I = Index->codegen();
        llvm::Type *Ty = llvm::IntegerType::getInt32Ty(TheContext);
        llvm::Type *TA = llvm::ArrayType::get(Ty, VarSymbol->getType()->getArrayType()->Size);
        llvm::Value *PtrArray = Builder.CreateConstGEP2_32(TA, VarValue, 0, 0);
        llvm::Value *Num = Builder.CreateGEP(PtrArray, I);
        ArgsV.push_back(Num);
      }
    }
    Builder.CreateCall(readFunc, ArgsV, "retRead");
  }else {
    LogError("error[all]: 'read' was not defined. You must import io library. Use 'import io'.");
    return nullptr;
  }
}

BasicType ReadExprAST::getResultingType() {
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// ReturnExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* ReturnExprAST::codegen() {
  if(Expr) {
    llvm::Value *ExpV = Expr->codegen();
    Builder.CreateRet(ExpV);
  }else {
    Builder.CreateRetVoid();
  }
}

BasicType ReturnExprAST::getResultingType() {
  return BasicType::Undefined;
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

BasicType CallExprAST::getResultingType() {
  std::shared_ptr<Symbol> SubSymbol = S->find(Callee);
  if(SubSymbol) {
    if(SubSymbol->getSymbolType() != SymbolType::Function) {
      std::string MsgError = "'" + Callee + "' is not a function";
      LogError(MsgError, yylineno);
      return BasicType::Undefined;
    }
    return SubSymbol->getType()->getPrimitiveType()->getBasicType();
  }
  std::string MsgError = "subrotina '" + Callee + "' não foi declarada no escopo corrente";
  LogError(MsgError, yylineno);
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// BlockExprAST
////===----------------------------------------------------------------------===//

llvm::Value* BlockExprAST::codegen() {
  for(int i = 0; i < Exps.size(); i++) {
    Exps[i]->codegen();
  }
  return llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
}

BasicType BlockExprAST::getResultingType() {
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// StopOrSkipExprAST
////===----------------------------------------------------------------------===//

llvm::Value* StopOrSkipExprAST::codegen() {}

BasicType StopOrSkipExprAST::getResultingType() {
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// PrototypeAST
////===----------------------------------------------------------------------===//

llvm::Function* PrototypeAST::codegen() {
  std::vector<llvm::Type*> ArgsVector;
  llvm::FunctionType *FT;

  for(int i = 0; i < Args.size(); i++) {
    auto T = S->find(Args[i])->getType();
    auto PType = T->getPrimitiveType();
    auto AType = T->getArrayType();
    //set args type
    switch(PType->getBasicType()) {
      case BasicType::Int:
        if(AType->isArray) {
          ArgsVector.push_back(llvm::Type::getInt32PtrTy(TheContext));
        }else {
          ArgsVector.push_back(llvm::Type::getInt32Ty(TheContext));
        }
        break;
      case BasicType::Bool:
        if(AType->isArray) {
          ArgsVector.push_back(llvm::Type::getInt1PtrTy(TheContext));
        }else {
          ArgsVector.push_back(llvm::Type::getInt1Ty(TheContext));
        }
        break;
      case BasicType::String:
          ArgsVector.push_back(llvm::Type::getInt8PtrTy(TheContext));
        break;
      default: 
        ArgsVector.push_back(llvm::Type::getVoidTy(TheContext));
    }
  }

  auto T = S->find(Name)->getType()->getPrimitiveType();

  switch(T->getBasicType()) {
    case BasicType::Int:
      FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(TheContext), ArgsVector, false);
      break;
    case BasicType::Bool:
      FT = llvm::FunctionType::get(llvm::Type::getInt1Ty(TheContext), ArgsVector, false);
    case BasicType::String:
      FT = llvm::FunctionType::get(Builder.getInt8PtrTy(), ArgsVector, false);
      break;
    default:
      FT = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), ArgsVector, false);
  }

  llvm::Function* F = llvm::Function::Create(
      FT, llvm::Function::ExternalLinkage, Name, TheModule.get());

  unsigned Idx = 0;
  for(auto &Arg : F->args()) {
    Arg.setName(Args[Idx]);
    Idx++;
  }

  return F;
}

//===------------------------------------------------------------------------===//
//// SubroutineAST
////===----------------------------------------------------------------------===//

llvm::Function* SubroutineAST::codegen() {
  //check symbol table
  llvm::Function *TheFunction = Proto->codegen();
  
  if(!TheFunction)
    return nullptr;
  
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);
  
  //NamedValues.clear();
  
  for(auto &Arg : TheFunction->args()) {
    //Create an alloca for this variable.
    llvm::AllocaInst *AllocaArgs = CreateEntryBlockAllocaArgs(TheFunction, Arg.getName());
    //Store the initial value into the alloca.
    Builder.CreateStore(&Arg, AllocaArgs);
    //Add arguments to variable symbol table.
    std::shared_ptr<Symbol> Symb = S->find(Arg.getName());
    auto VarSymbol = static_cast<VariableSymbol*>(Symb.get());
    VarSymbol->setValue(AllocaArgs);
    //NamedValues[Arg.getName()] = AllocaArgs;
  }
  
  if (llvm::Value *RetVal = Body->codegen()) {
    //Finish off the function.
    Builder.CreateRetVoid();
    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);
    // Run the optimizer on the function.
    return TheFunction;
  }
  
  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}
