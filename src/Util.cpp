#include "Util.hpp"

using namespace grc;

extern Scope S;

ExprAST* HandleExpression(const std::string &Op, ExprAST *Operand) {
  std::unique_ptr<ExprAST> UPOp(Operand);
  return new UnaryExprAST(Op, std::move(UPOp)); 
}

ExprAST* HandleExpression(const std::string &Op, ExprAST *ExprL, ExprAST *ExprR) {
  std::unique_ptr<ExprAST> UPExprL(ExprL);
  std::unique_ptr<ExprAST> UPExprR(ExprR);
  return new BinaryExprAST(Op, std::move(UPExprL), std::move(UPExprR)); 
}

ExprAST* HandleExpression(uint8_t Op, ExprAST* ExprL, ExprAST* ExprR) {
  std::unique_ptr<ExprAST> UPExprL(ExprL);
  std::unique_ptr<ExprAST> UPExprR(ExprR);
  switch(Op) {
    case 1:
      return new BinaryExprAST(">", std::move(UPExprL), std::move(UPExprR));
      break;
    case 2:
      return new BinaryExprAST("<", std::move(UPExprL), std::move(UPExprR));
      break;
    case 3:
      return new BinaryExprAST("<>", std::move(UPExprL), std::move(UPExprR));
      break;
    case 4:
      return new BinaryExprAST(">=", std::move(UPExprL), std::move(UPExprR));
      break;
    default:
      return new BinaryExprAST("<=", std::move(UPExprL), std::move(UPExprR));
  }
}

ExprAST* HandleCmdIf(ExprAST* Cond, ExprAST* Then, ExprAST* Else) {
  std::unique_ptr<ExprAST> UPCond(Cond);
  std::unique_ptr<ExprAST> UPThen(Then);
  std::unique_ptr<ExprAST> UPElse(Else);
  return new IfExprAST(std::move(UPCond), std::move(UPThen), std::move(UPElse));
}

ExprAST* HandleCmdIf(ExprAST* Cond, ExprAST* Then) {
  std::unique_ptr<ExprAST> UPCond(Cond);
  std::unique_ptr<ExprAST> UPThen(Then);
  return new IfExprAST(std::move(UPCond), std::move(UPThen), nullptr);
}

BlockAST* HandleCmd() {
  return new BlockAST();
}

void HandleCmd(BlockAST *Block, ExprAST *Expr) {
  std::unique_ptr<ExprAST> UPExpr(Expr);
  Block->addExprAST(std::move(UPExpr));
}

void HandlePrototype(const std::string Name, Parameters* Params) {
  std::unique_ptr<ProcedureSymbol> UNProc = std::make_unique<ProcedureSymbol>();
  S.insert(Name, std::move(UNProc));
  
  for(int i = 0; i < Params->ListOfParams.size(); i++) {
    Params->ListOfParams[i]->PrimitiveType->toPrint();
  }
  /*
  std::unique_ptr<VariableSymbol> V = std::make_unique<VariableSymbol>();
  std::unique_ptr<ProcedureSymbol> Q = std::make_unique<ProcedureSymbol>();
  S.insert("a", std::move(V));
  S.insert("b", std::move(P));
  S.initializeScope();
  */
}

/*
void HandlePrototype(const std::string Name, ReturnType Type) {
  std::unique_ptr<FunctionSymbol> P = std::make_unique<FunctionSymbol>();
  S.insert(Name, std::move(P));
} */

Parameter* HandleParameter(const std::string &Name, bool Array) {
  Parameter *Param = new Parameter();
  Param->Name = Name;
  Param->Array = Array;
  Param->Size = 0;
  return Param;
}

Parameters* HandleParameters() {
  return new Parameters();
}

void HandleParameters(Parameters* Params, Parameter* Param) {
  std::unique_ptr<Parameter> UPParam(Param);
  Params->ListOfParams.push_back(std::move(UPParam));
}

grc::Type* HandleType(Types T, int Size) {
  return new Type(T, Size);
}

Parameters* HandleListOfParams() {
  return new Parameters();
}

void HandleListOfParams(Parameters *ParamsNew, Parameters *ParamsOld, Type *T) {
  for(int i = 0; i < ParamsOld->ListOfParams.size(); i++) {
    ParamsOld->ListOfParams[i]->PrimitiveType = T;
  }
  while(ParamsOld->ListOfParams.size() > 0) {
    int N = ParamsOld->ListOfParams.size() - 1;
    ParamsNew->ListOfParams.push_back(std::move(ParamsOld->ListOfParams[N]));
    ParamsOld->ListOfParams.pop_back();
  }
}
