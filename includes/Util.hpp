#pragma once

#include "AST.hpp"
#include "Scope.hpp"
#include "Log.hpp"
#include "Error.hpp"
#include "SymbolTable.hpp"

#include <iostream>
#include <cstdint>

//===------------------------------------------------------------------------===//
//// Temporary Types 
////===----------------------------------------------------------------------===//

struct Parameter {
  std::string Name;
  grc::ArrayType *AType;
  grc::BasicType BT;
  int Size;
};

struct Parameters{
  std::vector<std::unique_ptr<Parameter>> ListOfParams;
};

struct Expressions {
  std::vector<std::unique_ptr<grc::ExprAST>> ListOfExprs;
};

struct VariableAndType{
  std::unique_ptr<grc::Var> V;
  std::shared_ptr<grc::ArrayType> T;
};

struct Variables {
  std::vector<std::unique_ptr<VariableAndType>> ListOfVars;
};

struct Integers {
  std::vector<int> ListOfInts;
};

struct Booleans {
  std::vector<bool> ListOfBools;
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
grc::AssignAST* HandleAssign(const std::string&, const std::string, grc::ExprAST*); 
grc::AssignAST* HandleAssign(const std::string&, uint8_t, grc::ExprAST*); 
grc::AssignAST* HandleAssign(uint8_t, const std::string&); 
grc::ExprAST* HandleCmdWhile(grc::ExprAST*, grc::ExprAST*);
Parameter* HandleParameter(const std::string&, bool); 
Parameters* HandleParameters(); 
void HandleParameters(Parameters*, Parameter*);
grc::PrimitiveType* HandleType(grc::BasicType, int); 
Parameters* HandleListOfParams();
void HandleListOfParams(Parameters*, Parameters*, grc::PrimitiveType*); 
grc::SubroutineAST* HandleSubroutine(grc::PrototypeAST*, grc::BlockExprAST*); 
Expressions* HandleCmdCall();
void HandleCmdCall(Expressions *, grc::ExprAST*);
grc::CallExprAST* HandleCmdCall(const std::string&, Expressions*);


Expressions* HandleCmdWrite(); 
void HandleCmdWrite(Expressions *Exprs, grc::ExprAST* Expr); 
void HandleCmdWrite(Expressions *Exprs, const std::string&); 
grc::WriteExprAST* HandleCmdWrite(Expressions *Exprs); 

//===------------------------------------------------------------------------===//
//// Prototype 
////===----------------------------------------------------------------------===//

grc::PrototypeAST* HandlePrototype(const std::string&, Parameters*); 
grc::PrototypeAST* HandlePrototype(const std::string&, Parameters*, 
    grc::PrimitiveType*); 

//===------------------------------------------------------------------------===//
//// Block 
////===----------------------------------------------------------------------===//

void HandleCmd(Expressions*, grc::ExprAST*); 
Expressions* HandleCmd();
grc::BlockExprAST* HandleBlock(Expressions*); 

//===------------------------------------------------------------------------===//
//// Variable Declaration
////===----------------------------------------------------------------------===//

grc::VarExprAST* HandleVarCmd(Variables*, grc::PrimitiveType*);
Variables* HandleListOfVar();
void HandleListOfVar(Variables*, VariableAndType*);
VariableAndType* HandleVar(const std::string&); 
VariableAndType* HandleVar(const std::string&, grc::ExprAST*);
VariableAndType* HandleVar(const std::string&,  int Size, grc::ExprAST*); 
VariableAndType* HandleVar(const std::string&, const std::string&);
Booleans* HandleListOfBool();
Integers* HandleListOfInt();
void HandleBool(Booleans*, bool);
grc::BooleansExprAST* HandleBool(Booleans*);
void HandleInt(Integers*, int);
grc::IntegersExprAST* HandleInt(Integers*);

//===------------------------------------------------------------------------===//
//// Erros 
////===----------------------------------------------------------------------===//

void HandleError(const std::string&); 
void VerifyMain(); 

//===------------------------------------------------------------------------===//
//// Import 
////===----------------------------------------------------------------------===//

void HandleImportIO(); 


