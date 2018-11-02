#pragma once

#include "llvm/IR/Constants.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"

#include <iostream>
#include <vector>
#include <memory>

namespace grc {
  enum Type {INT, STRING, BOOL};
  
  class ExprAST {
  public:
    virtual ~ExprAST() = default;
    virtual void toPrint() = 0; 
  };
  
  class NumberExprAST : public ExprAST {
    int Val;
  public:
    NumberExprAST(int Val) : Val(Val) {}
    void toPrint() override { std::cout << Val << ' '; } 
  };

  class BooleanExprAST: public ExprAST {
    bool Bool;
  public:
    BooleanExprAST(bool Bool) : Bool(Bool) {}
    void toPrint() override { std::cout << Bool << ' '; }
  };

  class VariableExprAST: public ExprAST {
    std::string Name;
  public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    void toPrint() override { std::cout << Name << ' '; }
  };

  class UnaryExprAST: public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> Operand;
  public:
    UnaryExprAST(char Op, std::unique_ptr<ExprAST> Operand) : Op(Op), Operand(std::move(Operand)) {}
    void toPrint() override { std::cout << Op << ' '; Operand->toPrint(); };
  };

  class BinaryExprAST : public ExprAST {
    std::string Op;
    std::unique_ptr<ExprAST> LHS, RHS;
  public:
    BinaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> LHS,
        std::unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    void toPrint() override { std::cout << Op << ' '; LHS->toPrint(); RHS->toPrint(); }
  };

  class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then, Else;
  public:
    IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then, 
        std::unique_ptr<ExprAST> Else) : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
    void toPrint() override { Cond->toPrint(); }
  };

  class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
  public:
    PrototypeAST(const std::string &name) :
      Name(name) {}
    const std::string &getName() const { return Name; };
    llvm::Function* codegen();
  };

  class BodyAST {
    std::vector<std::unique_ptr<ExprAST*>> Body;
  public:
   BodyAST(); 
  };

  class ProcedureAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<BodyAST> Body; 
  public:
    ProcedureAST(std::unique_ptr<PrototypeAST> Proto) :
    Proto(std::move(Proto)) {}
    llvm::Value* codegen(llvm::LLVMContext &TheContext);
  };
}
