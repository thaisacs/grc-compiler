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

namespace grc {
  enum Traversals {InOrder, PosOrder, PreOrder};
  
  class ExprAST {
  public:
    virtual ~ExprAST() = default;
    virtual void toPrint(std::ofstream&, Traversals) = 0; 
  };
  
  class NumberExprAST : public ExprAST {
    int Val;
  public:
    NumberExprAST(int Val) : Val(Val) {}
    void toPrint(std::ofstream&, Traversals) override; 
    llvm::Value* codegen(llvm::LLVMContext&);
  };

  class BooleanExprAST: public ExprAST {
    bool Bool;
  public:
    BooleanExprAST(bool Bool) : Bool(Bool) {}
    void toPrint(std::ofstream&, Traversals) override;
  };

  class VariableExprAST: public ExprAST {
    std::string Name;
  public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    void toPrint(std::ofstream&, Traversals) override;
  };

  class UnaryExprAST: public ExprAST {
    std::string Op;
    std::unique_ptr<ExprAST> Operand;
  public:
    UnaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> Operand) : 
      Op(Op), Operand(std::move(Operand)) {}
    void toPrint(std::ofstream&, Traversals) override;
  };

  class BinaryExprAST : public ExprAST {
    std::string Op;
    std::unique_ptr<ExprAST> LHS, RHS;
  public:
    BinaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> LHS,
        std::unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    void toPrint(std::ofstream&, Traversals) override;
  };

  class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then, Else;
  public:
    IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then, 
        std::unique_ptr<ExprAST> Else) : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
    void toPrint(std::ofstream&, Traversals) override;
  };

  class AssignAST : public ExprAST {
    std::string Op;
    std::shared_ptr<Symbol> Var;
    std::unique_ptr<ExprAST> Expr;
  public:
    AssignAST(const std::string Op, std::shared_ptr<Symbol> Var, std::unique_ptr<ExprAST> Expr) : 
      Op(Op), Var(Var), Expr(std::move(Expr)) {}
    void toPrint(std::ofstream&, Traversals) override;
  };

  class BlockAST : public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> Exps;
  public:
    BlockAST() {}; 
    //BlockAST(std::vector<std::unique_ptr<ExprAST>> cmds) : 
    //  cmds(std::move(cmds)) {}; 
    void addExprAST(std::unique_ptr<ExprAST>);
    void toPrint(std::ofstream&, Traversals) override; 
  };

  class PrototypeAST {
    std::string Name;
    //std::vector<std::string> Args;
  public:
    PrototypeAST(const std::string &name) :
      Name(name) {}
    const std::string &getName() const { return Name; };
    void toPrint(std::ofstream&);
    llvm::Function* codegen();
  };

  class ProcedureAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<BlockAST> Block; 
  public:
    ProcedureAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<BlockAST> Block) : 
      Proto(std::move(Proto)), Block(std::move(Block)) {}
    void toPrint(std::ofstream&, Traversals T);
    llvm::Value* codegen(llvm::LLVMContext&);
  };
}
