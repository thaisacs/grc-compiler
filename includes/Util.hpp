#pragma once

#include "AST.hpp"
#include "Scope.hpp"
#include "Log.hpp"
#include "SymbolTable.hpp"

#include <iostream>
#include <cstdint>

struct Parameter {
  std::string Name;
  grc::Type* T;
  bool isArray;
  int Size;
};

struct Parameters{
  std::vector<std::unique_ptr<Parameter>> ListOfParams;
};

//===------------------------------------------------------------------------===//
//// Expression 
////===----------------------------------------------------------------------===//

grc::ExprAST* HandleExpression(const std::string&, grc::ExprAST*, grc::ExprAST*);
grc::ExprAST* HandleExpression(const std::string&, grc::ExprAST*);
grc::ExprAST* HandleExpression(uint8_t, grc::ExprAST*, grc::ExprAST*);

//===------------------------------------------------------------------------===//
//// Cmds 
////===----------------------------------------------------------------------===//

grc::ExprAST* HandleCmdIf(grc::ExprAST*, grc::ExprAST*, grc::ExprAST*); 
grc::ExprAST* HandleCmdIf(grc::ExprAST*, grc::ExprAST*); 
grc::BlockAST* HandleCmd();
void HandleCmd(grc::BlockAST*, grc::ExprAST*); 

grc::AssignAST* HandleAssign(const std::string, const std::string, grc::ExprAST*); 
grc::AssignAST* HandleAssign(const std::string, uint8_t, grc::ExprAST*); 
grc::AssignAST* HandleAssign(uint8_t, const std::string); 

grc::ExprAST* HandleCmdWhile(grc::ExprAST*, grc::ExprAST*);

grc::PrototypeAST* HandlePrototype(const std::string, Parameters*); 
Parameter* HandleParameter(const std::string&, bool); 
Parameters* HandleParameters(); 
void HandleParameters(Parameters*, Parameter*);
grc::Type* HandleType(grc::PrimitiveType, int); 
Parameters* HandleListOfParams();
void HandleListOfParams(Parameters*, Parameters*, grc::Type*); 
grc::ProcedureAST* HandleProcedure(grc::PrototypeAST*, grc::BlockAST*); 

grc::VarExprAST* HandleListOfVar();
void HandleListOfVar(grc::VarExprAST*, grc::Variable*);
grc::Variable* HandleVar(const std::string&); 
void HandleVarCmd(grc::VarExprAST*, grc::Type*);

grc::Variable* HandleVar(const std::string &Name, grc::ExprAST* Expr);
