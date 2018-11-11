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
  class ExprAST {
  public:
    virtual ~ExprAST() = default;
    virtual llvm::Value* codegen() = 0;
    virtual void toPrint(std::ofstream&) = 0; 
  };
  
  class NumberExprAST : public ExprAST {
    int Val;
  public:
    NumberExprAST(int Val) : Val(Val) {}
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override; 
  };
/*
  class NumbersExprAST : public ExprAST {
    std::vector<int> Numbers;
  public:
    NumbersExprAST() {}
    void addNumber(int Number);
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };
*/
  class BooleanExprAST: public ExprAST {
    bool Bool;
  public:
    BooleanExprAST(bool Bool) : Bool(Bool) {}
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };

  class VariableExprAST: public ExprAST {
    std::string Name;
  public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };
/*
  class ReadExprAST: public ExprAST {
    std::string Name;  
  public:
    ReadExprAST(cont std::string &Name) : Name(Name);
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };

  class WriteExprAST: public ExprAST {
    std::string Name;  
  public:
    WriteExprAST(cont std::string &Name) : Name(Name);
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };
*/
  class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
  public:
    CallExprAST(const std::string &Callee, 
        std::vector<std::unique_ptr<ExprAST>> Args) : 
      Callee(Callee), Args(std::move(Args)) {}
  };

  class UnaryExprAST: public ExprAST {
    std::string Op;
    std::unique_ptr<ExprAST> Operand;
  public:
    UnaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> Operand) : 
      Op(Op), Operand(std::move(Operand)) {}
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };

  class BinaryExprAST : public ExprAST {
    std::string Op;
    std::unique_ptr<ExprAST> LHS, RHS;
  public:
    BinaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> LHS,
        std::unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };

  class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then, Else;
  public:
    IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then, 
        std::unique_ptr<ExprAST> Else) : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };

  class WhileExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Block;  
  public:
    WhileExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Block) : 
      Cond(std::move(Cond)), Block(std::move(Block)) {}
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };

  class ForExprAST : public ExprAST {};

  class AssignAST : public ExprAST {
    std::string Op;
    std::shared_ptr<Symbol> Var;
    std::unique_ptr<ExprAST> Expr;
  public:
    AssignAST(const std::string Op, std::shared_ptr<Symbol> Var, std::unique_ptr<ExprAST> Expr) : 
      Op(Op), Var(Var), Expr(std::move(Expr)) {}
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };

  class Var {
    std::string Name;
    std::unique_ptr<ExprAST> Expr;
  public:
    Var(const std::string Name, std::unique_ptr<ExprAST> Expr) : 
      Name(Name), Expr(std::move(Expr)) {}
    void toPrint(std::ofstream&);
  };

  class VarExprAST : public ExprAST {
    std::vector<std::unique_ptr<Var>> Vars;
    std::unique_ptr<Type> PrimitiveType;
  public:
    VarExprAST() {}
    void addVar(std::unique_ptr<Var>);
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override;
  };

  class BlockAST : public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> Exps;
  public:
    BlockAST() {}; 
    void addExprAST(std::unique_ptr<ExprAST> Exp);
    llvm::Value* codegen() override;
    void toPrint(std::ofstream&) override; 
  };

  class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
  public:
    PrototypeAST(const std::string &name, std::vector<std::string> Args) : 
      Name(name), Args(Args) {}
    const std::string &getName() const { return Name; };
    void toPrint(std::ofstream&);
    llvm::Function* codegen();
  };

  class ProcedureAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<BlockAST> Body; 
  public:
    ProcedureAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<BlockAST> Body) : 
      Proto(std::move(Proto)), Body(std::move(Body)) {}
    void toPrint(std::ofstream&);
    llvm::Function* codegen();
  };
}
