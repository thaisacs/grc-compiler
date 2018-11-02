#pragma once

#include "llvm/IR/Constants.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include <iostream>
#include <vector>
#include <memory>

namespace compiler {
  enum Type {INT, STRING, BOOL};
 
  class ExprAST {
  public:
    virtual ~ExprAST() = default;
  };
  
  class NumberExprAST : public ExprAST {
    int Val;
  public:
    NumberExprAST(int Val) : Val(Val) {}
  };

  class BinaryExprAST : public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
  public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
        std::unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  };

  //class DecVarAST {
  //  std::vector<std::string> Names;
  //  Type T;
  //public:
  //  DecVarAST(std::vector<std::string> Names, Type T) : 
  //    Names(Names), T(T) {}
  //  llvm::GlobalVariable* codegen(llvm::LLVMContext &TheContext);
  //};
  
  class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
  public:
    PrototypeAST(const std::string &name) :
      Name(name) {}
    const std::string &getName() const { return Name; };
    llvm::Function* codegen();
  };
  
  class ProcedureAST {
    std::unique_ptr<PrototypeAST> Proto;
  public:
    ProcedureAST(std::unique_ptr<PrototypeAST> Proto) :
    Proto(std::move(Proto)) {}
    llvm::Value* codegen(llvm::LLVMContext &TheContext);
  };
}
