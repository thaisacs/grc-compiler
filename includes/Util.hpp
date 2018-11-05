#pragma once

#include "AST.hpp"
#include "Scope.hpp"
#include "SymbolTable.hpp"

#include <iostream>
#include <cstdint>

struct Parameter {
  std::string Name;
  grc::Type* T;
  bool Array;
  int Size;
};

struct Parameters{
  std::vector<std::unique_ptr<Parameter>> ListOfParams;
};

grc::ExprAST* HandleExpression(const std::string &Op, grc::ExprAST* ExprA, grc::ExprAST* ExprB);
grc::ExprAST* HandleExpression(const std::string &Op, grc::ExprAST *Operand);
grc::ExprAST* HandleExpression(uint8_t Op, grc::ExprAST* ExprA, grc::ExprAST* ExprB);
grc::ExprAST* HandleCmdIf(grc::ExprAST* Cond, grc::ExprAST* Then, grc::ExprAST* Else); 
grc::ExprAST* HandleCmdIf(grc::ExprAST* Cond, grc::ExprAST* Then); 
grc::BlockAST* HandleCmd();
void HandleCmd(grc::BlockAST *block, grc::ExprAST *Expr); 
void HandlePrototype(const std::string Name, Parameters* Param); 

//void HandlePrototype(const std::string Name, grc::ReturnType Type); 
Parameter* HandleParameter(const std::string &Name, bool Array); 
Parameters* HandleParameters(); 
void HandleParameters(Parameters* Params, Parameter* Param);
grc::Type* HandleType(grc::Types T, int Size); 
Parameters* HandleListOfParams();
void HandleListOfParams(Parameters *ParamsNew, Parameters *ParamsOld, grc::Type *T); 
