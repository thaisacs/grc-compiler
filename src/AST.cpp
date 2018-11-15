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

#include "llvm/IR/NoFolder.h"

#include "Scope.hpp"
#include "Log.hpp"

using namespace grc;

extern llvm::LLVMContext TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
extern std::unique_ptr<Log> LOG;
extern std::shared_ptr<Scope> S;
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
        llvm::Type* I = llvm::IntegerType::getDoubleTy(TheContext);
        //llvm::ArrayType* arrayType = llvm::ArrayType::get(I, 5);
        return TmpB.CreateAlloca(llvm::ArrayType::get(I, AType->Size), 0, 
          VarName.c_str());
      }else {
        return TmpB.CreateAlloca(llvm::Type::getDoubleTy(TheContext), 0, 
          VarName.c_str());
      }
      break;
    case BasicType::Bool:
      if(AType->isArray) {
        llvm::Type* I = llvm::IntegerType::getInt8Ty(TheContext);
        //llvm::ArrayType* arrayType = llvm::ArrayType::get(I, 5);
        return TmpB.CreateAlloca(llvm::ArrayType::get(I, AType->Size), 0, 
          VarName.c_str());
      }else {
        return TmpB.CreateAlloca(llvm::Type::getInt8Ty(TheContext), 0, 
          VarName.c_str());
      }
     break;
    case BasicType::String:
      //ArgsVector.push_back(llvm::Type::getInt8PtrTy(TheContext));
      break;
    //default: // BasicType::Undefined and BasicType::Void
      //ArgsVector.push_back(llvm::Type::getVoidTy(TheContext));
  } 
}

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
  llvm::Type* I = llvm::IntegerType::getDoubleTy(TheContext);
  llvm::ArrayType* arrayType = llvm::ArrayType::get(I, Ints.size());

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

llvm::Value* BooleansExprAST::codegen() {}

//===------------------------------------------------------------------------===//
//// NumberExprAST 
////===----------------------------------------------------------------------===//

void NumberExprAST::toPrint(std::ofstream &File) {
  File << Val;
};

llvm::Value* NumberExprAST::codegen() {
  double V = (double) Val;
  return llvm::ConstantFP::get(TheContext, llvm::APFloat(V));
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

llvm::Value* BooleanExprAST::codegen() {
  if(Bool)
    return llvm::ConstantInt::get(TheContext, llvm::APInt(8, 1));
  else
    return llvm::ConstantInt::get(TheContext, llvm::APInt(8, 0));
}

//===------------------------------------------------------------------------===//
//// StringExprAST 
////===----------------------------------------------------------------------===//

void StringExprAST::toPrint(std::ofstream &File) {
  File << String;
}

llvm::Value* StringExprAST::codegen() {
  std::string Str;
  
  unsigned i = 0;
  while(i < String.size()) {
    if(String[i] == '\%' && i < String.size() -1 && String[i+1] == 'b') {
      Str += "%d";
      i++;
    }else if(String[i] == '\%' && i < String.size() -1 && String[i+1] == 'd') {
      Str += "%.0f";
      i++;
    }else if(String[i] != '\"') {
      Str +=  String[i];
    }    
    i++;
  }
  Str += "\n"; 
  //std::string Str = String.substr(1, String.size()-2) + "\n";
  llvm::Value *StrV = Builder.CreateGlobalStringPtr(Str);
  return StrV;
}

//===------------------------------------------------------------------------===//
//// VariableExprAST 
////===----------------------------------------------------------------------===//

void VariableExprAST::toPrint(std::ofstream &File) {
  File << Name;
}

llvm::Value* VariableExprAST::codegen() {
  // Look this variable up in the function.
  llvm::Value *V = NamedValues[Name];
  if (!V) {
    LogError("error: Unknown variable name");
    return nullptr;
  }
  // Load the value.
  return Builder.CreateLoad(V, Name.c_str());
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
   
  if(Op == "+"){
    return Builder.CreateFAdd(L, R, "addtmp");
  }else if(Op == "-") {
    return Builder.CreateFSub(L, R, "subtmp");
  }else if(Op == "*") {
    return Builder.CreateFMul(L, R, "multmp");
  }else if(Op == "/") {
    return Builder.CreateFDiv(L, R, "divtmp");
  }else if(Op == "<") {
    L = Builder.CreateFCmpULT(L, R, "cmptmp");
    return Builder.CreateUIToFP(L, llvm::Type::getInt32Ty(TheContext), "booltmp");
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
  File << Name << " ";
  //if(Expr)
  //  Expr->toPrint(File);
}

llvm::Value* VarExprAST::codegen() {
  std::vector<llvm::AllocaInst*> OldBindings;
  llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
  
  for(unsigned i = 0, e = Vars.size(); i != e; ++i) {
    const std::string &VarName = Vars[i]->getName();
    llvm::Value *InitVal;
    
    std::shared_ptr<Type> T = S->find(VarName)->getType();
    auto AType = T->getArrayType();
    auto PType = T->getPrimitiveType();
    
    if(AType->isArray) {
      llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
      InitVal =  llvm::ConstantInt::get(TheContext, llvm::APInt(8, 5));
      

      OldBindings.push_back(NamedValues[VarName]);
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
            InitVal = llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0));
            break;
          case BasicType::Bool:
            InitVal =  llvm::ConstantInt::get(TheContext, llvm::APInt(8, 0));
            break;
          case BasicType::String:
            break;
        }
      }
      llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
      Builder.CreateStore(InitVal, Alloca);

      OldBindings.push_back(NamedValues[VarName]);
      NamedValues[VarName] = Alloca;
    }
  } 
}

void VarExprAST::toPrint(std::ofstream &File) {
  File << "Var: ";
  for(int i = 0; i < Vars.size(); i++) {
    Vars[i]->toPrint(File);
    if(i < Vars.size() -1) {
      File << ",";
    }
  }
  File << "\n";
}

//===------------------------------------------------------------------------===//
//// WriteExprAST 
////===----------------------------------------------------------------------===//

llvm::Value* WriteExprAST::codegen() {
  llvm::Function* putsFunc = TheModule->getFunction("printf");
  
  if(putsFunc) {
    std::vector<llvm::Value*> ArgsV;
    
    for(unsigned i = 0; i < Args.size(); i++) {
      //if (llvm::ConstantFP* F = llvm::dyn_cast<llvm::ConstantFP>(Args[i]->codegen())) {
      //  F->getValueAPF().convertToInteger();
      //}else {
      //}
      ArgsV.push_back(Args[i]->codegen());
      
    }
    
    Builder.CreateCall(putsFunc, ArgsV, "retWrite");
  }else {
    LogError("error[all]: write function not fount. You must import io.");
    return nullptr;
  }
}

void WriteExprAST::toPrint(std::ofstream &File) {
}

//===------------------------------------------------------------------------===//
//// ReturnExprAST 
////===----------------------------------------------------------------------===//

void ReturnExprAST::toPrint(std::ofstream &File) {}

llvm::Value* ReturnExprAST::codegen() {}

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
  File << "Block:\n";
  for(int i = 0; i < Exps.size(); i++) {
    Exps[i]->toPrint(File);
  }
}

llvm::Value* BlockExprAST::codegen() {
  for(int i = 0; i < Exps.size(); i++) {
    Exps[i]->codegen();
  }
  return llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
}

//===------------------------------------------------------------------------===//
//// PrototypeAST
////===----------------------------------------------------------------------===//

void PrototypeAST::toPrint(std::ofstream &File) {
  File << "Prototype: Name(" << Name << ") e Args( ";
  for(int i = 0; i < Args.size(); i++) {
    File << Args[i];
    if(i < Args.size() - 1) {
      File << ",";
    }
  }
  File << ")" << std::endl;
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
          ArgsVector.push_back(llvm::Type::getInt8PtrTy(TheContext));
        }else {
          ArgsVector.push_back(llvm::Type::getInt8Ty(TheContext));
        }
       break;
      case BasicType::String:
        //ArgsVector.push_back(llvm::Type::getInt8PtrTy(TheContext));
        break;
      default: // BasicType::Undefined and BasicType::Void
        ArgsVector.push_back(llvm::Type::getVoidTy(TheContext));
    }
  }
  
  auto T = S->find(Name)->getType()->getPrimitiveType();
  
  switch(T->getBasicType()) {
    case BasicType::Int:
      FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(TheContext), ArgsVector, false);
      break;
    case BasicType::Bool:
      FT = llvm::FunctionType::get(llvm::Type::getInt8Ty(TheContext), ArgsVector, false);
    case BasicType::String:
      break;
    default:
      FT = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), ArgsVector, false);
  }
  
  llvm::Function* F = llvm::Function::Create(
      FT, llvm::Function::ExternalLinkage, Name, TheModule.get());
  
  unsigned Idx = 0;
  for(auto &Arg : F->args())
    Arg.setName(Args[Args.size() - 1 - Idx++]);

  //return F;


 // std::vector<llvm::Type *> Doubles(Args.size(), llvm::Type::getInt32Ty(TheContext));
 // llvm::FunctionType *FT =
 // llvm::FunctionType::get(llvm::Type::getInt32Ty(TheContext), Doubles, false);
 // llvm::Function *F =
 // llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Name, TheModule.get());
 // unsigned Idx = 0;
 // for (auto &Arg : F->args())
 // Arg.setName(Args[Idx++]);
  return F;
}

//===------------------------------------------------------------------------===//
//// SubroutineAST
////===----------------------------------------------------------------------===//

void SubroutineAST::toPrint(std::ofstream &File) {
  File << "#ProcedureAST\n";
  Proto->toPrint(File);
  Body->toPrint(File); 
  File << "#\n\n";
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
  //  S->setVariableValue((std::string)Arg.getName(),(llvm::Value*) &Arg);
  }
  
  if (llvm::Value *RetVal = Body->codegen()) {
    //Finish off the function.
    Builder.CreateRet(nullptr);
    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);
    // Run the optimizer on the function.
    //TheFPM->run(*TheFunction);
    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}
