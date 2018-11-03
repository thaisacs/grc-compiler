#pragma once

#include "AST.hpp"
#include <cstdint>

grc::ExprAST* HandleExpression(const std::string &Op, grc::ExprAST* ExprA, 
    grc::ExprAST* ExprB);

grc::ExprAST* HandleExpression(const std::string &Op, grc::ExprAST *Operand);

grc::ExprAST* HandleExpression(uint8_t Op, grc::ExprAST* ExprA, 
    grc::ExprAST* ExprB);

grc::ExprAST* HandleCmdIf(grc::ExprAST* Cond, grc::ExprAST* Then, 
    grc::ExprAST* Else); 

grc::ExprAST* HandleCmdIf(grc::ExprAST* Cond, grc::ExprAST* Then); 

grc::BlockAST* HandleCmd();

void HandleCmd(grc::BlockAST *block, grc::ExprAST *Expr); 
