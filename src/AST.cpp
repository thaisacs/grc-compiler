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
extern std::unique_ptr<Log> LOG;
extern std::shared_ptr<Scope> S;

extern int yylineno;

llvm::IRBuilder<> Builder(TheContext);

//===------------------------------------------------------------------------===//
//// Variable IR
////===----------------------------------------------------------------------===//

static std::map<std::string, llvm::AllocaInst*> NamedValues;

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
  // Look this variable up in the function.
  std::shared_ptr<Symbol> VarSymbol = S->find(Name);
  if(VarSymbol) {
    if(VarSymbol->getType()->getPrimitiveType()->getBasicType() ==
        BasicType::String) {
      llvm::Value *V = NamedValues[Name];
      llvm::Type *Ty = llvm::IntegerType::getInt8Ty(TheContext);
      llvm::Type *TA = llvm::ArrayType::get(Ty, 
          VarSymbol->getType()->getPrimitiveType()->getSize() + 1);
      return Builder.CreateConstGEP2_32(TA, V, 0, 0);
    }
  } 
  
  if(!VarSymbol->getType()->getArrayType()->isArray) {
    llvm::Value *V = NamedValues[Name];
    if (!V)
      return nullptr;
    // Load the value.
    return Builder.CreateLoad(V, Name.c_str());
  }else {
    llvm::Value *V = NamedValues[Name];
    llvm::Value *I = Index->codegen();
    llvm::Value *VI, *PtrVec; 
    llvm::Type *Ty, *TA;
    std::shared_ptr<Symbol> VarSymbol = S->find(Name);
    if(VarSymbol) {
      switch(VarSymbol->getType()->getPrimitiveType()->getBasicType()) {
        case BasicType::Int:
          Ty = llvm::IntegerType::getInt32Ty(TheContext);
          TA = llvm::ArrayType::get(Ty, VarSymbol->getType()->getArrayType()->Size);
          PtrVec = Builder.CreateConstGEP2_32(TA, V, 0, 0);
          VI = Builder.CreateGEP(PtrVec, I);
          break;
        case BasicType::Bool:
          Ty = llvm::IntegerType::getInt1Ty(TheContext);
          TA = llvm::ArrayType::get(Ty, VarSymbol->getType()->getArrayType()->Size);
          PtrVec = Builder.CreateConstGEP2_32(TA, V, 0, 0);
          VI = Builder.CreateGEP(PtrVec, I);
          break;
      }
      if(!VI)
        return nullptr;
      // Load the value.
      return Builder.CreateLoad(VI, Name.c_str());
    }
  }
}

BasicType VariableExprAST::getResultingType() {
  std::shared_ptr<Symbol> VarSymbol = S->find(Name);
  if(VarSymbol) {
    switch(VarSymbol->getType()->getPrimitiveType()->getBasicType()) {
      case BasicType::Int:
        if(VarSymbol->getType()->getArrayType()->isArray) {
          return BasicType::IntArray;
        }else {
          return BasicType::Int;
        }
      case BasicType::Bool:
        if(VarSymbol->getType()->getArrayType()->isArray) {
          return BasicType::BoolArray;
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

  std::string MsgError = "variável '" + Name + "' não foi declarada no escopo corrente";
  LogError(MsgError, yylineno);
  return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// UnaryExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* UnaryExprAST::codegen() {
  llvm::Value *R = Operand->codegen();
  if(Op == "!") {
  }else {
    return Builder.CreateNeg(R, "tmpnot");
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
    return Builder.CreateICmpULT(L, R, "resultmp");
  }else if(Op == ">") {
    return Builder.CreateICmpUGT(L, R, "cmptmp");
  }else if(Op == ">=") {
    return Builder.CreateICmpUGE(L, R, "cmptmp");
  }else if(Op == "<=") {
    return Builder.CreateICmpULE(L, R, "cmptmp");
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
  //llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

  //llvm::BasicBlock *CondBB = 
  //  llvm::BasicBlock::Create(TheContext, "condloop", TheFunction); 
  //llvm::BasicBlock *LoopBB = 
  //  llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
  //llvm::BasicBlock *AfterBB = 
  //  llvm::BasicBlock::Create(TheContext, "afterloop", TheFunction);

  //Builder.CreateBr(CondBB);

  //Builder.SetInsertPoint(CondBB);
  //
  //llvm::Value *CondV = Cond->codegen();
  //if(!Cond)
  //  return nullptr;

  //Builder.CreateCondBr(CondV, LoopBB, AfterBB);

  //Builder.SetInsertPoint(LoopBB);
  //if(!Block->codegen())
  //  return nullptr;
  //Builder.CreateBr(CondBB);

  //Builder.SetInsertPoint(AfterBB);
}

BasicType WhileExprAST::getResultingType() {
  //return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// AssignExprAST
////===----------------------------------------------------------------------===//

llvm::Value* AssignExprAST::codegen() {
  //llvm::Value *Val = Expr->codegen(); 
  //
  //if(!Val)
  //  return nullptr;

  //llvm::Value *Variable = NamedValues[Name];
  //if (!Variable)
  //  return nullptr;
  //  
  //llvm::Value *OldVal = Builder.CreateLoad(Variable, Name.c_str());
  //llvm::Value *Result;

  //if(Op == "=") {
  //  Result = Val;
  //}else if(Op == "+=") {
  //  Result = Builder.CreateAdd(OldVal, Val, "resultmp");
  //}else if(Op == "-=") {
  //  Result = Builder.CreateSub(OldVal, Val, "resultmp");
  //}else if(Op == "*=") {
  //  Result = Builder.CreateMul(OldVal, Val, "resultmp");
  //}else if(Op == "/=") {
  //  llvm::Value *Result = Builder.CreateSDiv(OldVal, Val, "resultmp");
  //}else if(Op == "%=") {
  //  llvm::Value *RD = Builder.CreateSDiv(OldVal, Val, "resultmp");
  //  llvm::Value *Result1 = Builder.CreateMul(RD, Val, "resultmp");
  //  Result1 = Builder.CreateSub(OldVal, Result1, "resultmp");
  //} 
  //
  //Builder.CreateStore(Result, Variable);
  //return Result;;
}

BasicType AssignExprAST::getResultingType() {
  //return BasicType::Undefined;
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

            NamedValues[VarName] = Alloca;
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

            NamedValues[VarName] = Alloca;
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

            NamedValues[VarName] = Alloca;
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
            NamedValues[VarName] = Alloca;
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
      NamedValues[VarName] = Alloca;
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
      NamedValues[VarName] = Alloca;
    }
  } 
}

BasicType VarExprAST::getResultingType() {
  //return BasicType::Undefined;
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
  //return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// ReadExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* ReadExprAST::codegen() {
  //std::vector<llvm::Value*> ArgsV;
  //llvm::Function* readFunc = TheModule->getFunction("scanf");
  //llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
  //if(readFunc) {
  //  llvm::Value *formatStr = Builder.CreateGlobalStringPtr("%d");
  //  ArgsV.push_back(formatStr);
  //  if(Index == -1) {
  //    ArgsV.push_back(NamedValues[Name]);
  //  }else {
  //    llvm::Value *V = NamedValues[Name];
  //    llvm::Value *I = llvm::ConstantInt::get(TheContext, llvm::APInt(32, Index));
  //    llvm::Value *VI = Builder.CreateGEP(V, I);
  //    ArgsV.push_back(VI);
  //  }
  //  Builder.CreateCall(readFunc, ArgsV, "retRead");
  //}else {
  //  LogError("error[all]: função read não foi encontrada. Você deve importar a biblioteca io.");
  //  return nullptr;
  //}
}

BasicType ReadExprAST::getResultingType() {
  //return BasicType::Undefined;
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
      std::string MsgError = "'" + Callee + "' não é uma função";
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

void BlockExprAST::print() {
}

llvm::Value* BlockExprAST::codegen() {
  for(int i = 0; i < Exps.size(); i++) {
    Exps[i]->codegen();
  }
  return llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
}

BasicType BlockExprAST::getResultingType() {
  //return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// StopOrSkipExprAST
////===----------------------------------------------------------------------===//

llvm::Value* StopOrSkipExprAST::codegen() {
  //llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
  //
  //if(Cmd == BasicItCmd::Skip) {
  //}else {
  //}
}

BasicType StopOrSkipExprAST::getResultingType() {
  //return BasicType::Undefined;
}

//===------------------------------------------------------------------------===//
//// PrototypeAST
////===----------------------------------------------------------------------===//

void PrototypeAST::print() {
  std::cout << Name << std::endl;
  for(unsigned i = 0; i < Args.size(); i++) {
    std::cout << Args[i] << std::endl;
  }
}

llvm::Function* PrototypeAST::codegen() {

  std::vector<llvm::Type*> ArgsVector;
  llvm::FunctionType *FT;

  for(int i = Args.size()-1; i >= 0; i--) {
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

void SubroutineAST::print() {
}

llvm::Function* SubroutineAST::codegen() {
  //check symbol table
  llvm::Function *TheFunction = Proto->codegen();
  
  if(!TheFunction)
    return nullptr;

  llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  NamedValues.clear();
  for(auto &Arg : TheFunction->args()) {
    //Create an alloca for this variable.
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());
    //Store the initial value into the alloca.
    Builder.CreateStore(&Arg, Alloca);
    //Add arguments to variable symbol table.
    NamedValues[Arg.getName()] = Alloca;
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



