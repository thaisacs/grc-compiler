#pragma once

#include "llvm/IR/Constants.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"

#include "SymbolTable.hpp"

#include <iostream>
#include <vector>
#include <memory>
#include <fstream>
#include <tuple>

namespace grc {
  enum BasicItCmd { Skip, Stop };
  
  class ExprAST {
  public:
    virtual ~ExprAST() = default;
    virtual llvm::Value* codegen() = 0;
    virtual BasicType getResultingType() = 0;
  };
  
  class NumberExprAST : public ExprAST {
    int Val;
  public:
    NumberExprAST(int Val) : Val(Val) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class BooleanExprAST: public ExprAST {
    bool Bool;
  public:
    BooleanExprAST(bool Bool) : Bool(Bool) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class IntegersExprAST: public ExprAST {
    std::vector<int> Ints;
  public:
    IntegersExprAST(std::vector<int> Ints) : Ints(std::move(Ints)) {}
    std::vector<int> getInts() { return Ints; }
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };
  
  class BooleansExprAST: public ExprAST {
    std::vector<bool> Bools;
  public:
    BooleansExprAST(std::vector<bool> Bools) : Bools(std::move(Bools)) {}
    std::vector<bool> getBools() { return Bools; }
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class StringExprAST: public ExprAST {
    std::string String;
  public:
    StringExprAST(const std::string &String) : String(String) {}
    std::string getString() { return String; }
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class VariableExprAST: public ExprAST {
    std::string Name;
    std::unique_ptr<ExprAST> Index;
  public:
    VariableExprAST(const std::string &Name, std::unique_ptr<ExprAST> Index) : 
      Name(Name), Index(std::move(Index)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
    std::string getName() { return Name; }
  };

  class ReadExprAST: public ExprAST {
    std::string Name;
    std::unique_ptr<ExprAST> Index;
  public:
    ReadExprAST(const std::string &Name, std::unique_ptr<ExprAST> Index) : 
      Name(Name), Index(std::move(Index)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class WriteExprAST: public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> Args;
  public:
    WriteExprAST(std::vector<std::unique_ptr<ExprAST>> Args) : 
      Args(std::move(Args)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class ReturnExprAST: public ExprAST {
    std::unique_ptr<ExprAST> Expr;
  public:
    ReturnExprAST(std::unique_ptr<ExprAST> Expr) : Expr(std::move(Expr)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
  public:
    CallExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args) : 
      Callee(Callee), Args(std::move(Args)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class UnaryExprAST: public ExprAST {
    std::string Op;
    std::unique_ptr<ExprAST> Operand;
  public:
    UnaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> Operand) : 
      Op(Op), Operand(std::move(Operand)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class BinaryExprAST : public ExprAST {
    std::string Op;
    std::unique_ptr<ExprAST> LHS, RHS;
  public:
    BinaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> LHS,
        std::unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class TernaryExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Test;
    std::unique_ptr<ExprAST> Then;
    std::unique_ptr<ExprAST> Else;
  public:
    TernaryExprAST(std::unique_ptr<ExprAST> Test, std::unique_ptr<ExprAST> Then,
        std::unique_ptr<ExprAST> Else) : Test(std::move(Test)), Then(std::move(Then)), Else(std::move(Else)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then, Else;
  public:
    IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then, 
        std::unique_ptr<ExprAST> Else) : 
      Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class WhileExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Block;  
  public:
    WhileExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Block) : 
      Cond(std::move(Cond)), Block(std::move(Block)) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class AssignExprAST : public ExprAST {
    std::unique_ptr<VariableExprAST> Var;
    std::string Op;
    std::unique_ptr<ExprAST> Expr;
  public:
    AssignExprAST(const std::string Op, std::unique_ptr<VariableExprAST> Var, 
        std::unique_ptr<ExprAST> Expr) : Op(Op), Var(std::move(Var)), Expr(std::move(Expr)) {}
    llvm::Value* codegen() override;
    VariableExprAST* getVar() { return Var.get(); }
    ExprAST* getExpr() { return Expr.get(); }
    BasicType getResultingType();
  };

  class StopOrSkipExprAST : public ExprAST {
    BasicItCmd Cmd; 
  public:
    StopOrSkipExprAST(BasicItCmd Cmd) : Cmd(Cmd) {}
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class ForExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Start, End, Step, Body;
  public:
    ForExprAST(std::unique_ptr<ExprAST> Start, std::unique_ptr<ExprAST> End, 
        std::unique_ptr<ExprAST> Step, std::unique_ptr<ExprAST> Body) : 
      Start(std::move(Start)), End(std::move(End)), Body(std::move(Body)), Step(std::move(Step)) {
      }
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class VarExprAST : public ExprAST {
    std::vector<std::unique_ptr<AssignExprAST>> Vars;
  public:
    VarExprAST(std::vector<std::unique_ptr<AssignExprAST>> Vars) : Vars(std::move(Vars)) {}
    llvm::Value* globalCodegen(); 
    llvm::Value* codegen() override;
    BasicType getResultingType();
  };

  class BlockExprAST : public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> Exps;
  public:
    BlockExprAST(std::vector<std::unique_ptr<ExprAST>> Exps) : Exps(std::move(Exps)) {}; 
    llvm::Value* codegen() override;
    BasicType getResultingType();
    void print();
  };

  class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
  public:
    PrototypeAST(const std::string &name, std::vector<std::string> Args) : 
      Name(name), Args(Args) {}
    const std::string &getName() const { return Name; };
    llvm::Function* codegen();
    void print();
  };

  class SubroutineAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<BlockExprAST> Body; 
  public:
    SubroutineAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<BlockExprAST> Body) : 
      Proto(std::move(Proto)), Body(std::move(Body)) {}
    llvm::Function* codegen();
    void print();
  };
}
