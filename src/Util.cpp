#include "Util.hpp"

using namespace grc;

extern std::shared_ptr<grc::Scope> S;
extern std::unique_ptr<grc::Log> LOG;

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

ExprAST* HandleCmdWhile(ExprAST* Cond, ExprAST* Block) {
  std::unique_ptr<ExprAST> UPCond(Cond);
  std::unique_ptr<ExprAST> UPBlock(Block);
  return new WhileExprAST(std::move(UPCond), std::move(UPBlock));
}

PrototypeAST* HandlePrototype(const std::string Name, Parameters* Params) {
  //std::vector<std::string> Args;
  ////insert procedure in symbol table
  //std::unique_ptr<ProcedureSymbol> UPProc = std::make_unique<ProcedureSymbol>();
  //S->insert(Name, std::move(UPProc));
  ////start new scope
  //S->initializeScope();
  ////insert args in symbol table
  //for(int i = 0; i < Params->ListOfParams.size(); i++) {
  //  //instantiate one type for each variable
  //  std::unique_ptr<Type> UPType(Params->ListOfParams[i]->T->copy());
  //  //create variable with your type (primitive type and isArray {true or false})
  //  std::shared_ptr<VariableSymbol> SPVar = 
  //    std::make_shared<VariableSymbol>(std::move(UPType), Params->ListOfParams[i]->isArray);
  //  S->insert(Params->ListOfParams[i]->Name, std::move(SPVar));
  //  Args.push_back(Params->ListOfParams[i]->Name);
  //  //memory free
  //  if(i == Params->ListOfParams.size() - 1)
  //    delete Params->ListOfParams[i]->T;
  //}
  //delete Params;
  ////refresh log
  //LOG->scopes(S);
  ////create prototypeAST node
  //return new PrototypeAST(Name, Args); 
}

PrototypeAST* HandlePrototype(const std::string Name, Parameters* Params, Type* T) {
  //std::vector<std::string> Args;
  //std::unique_ptr<Type> UPT(T);
  //std::unique_ptr<FunctionSymbol> UPFunc = std::make_unique<FunctionSymbol>(std::move(UPT));
  //S->insert(Name, std::move(UPFunc));
  //S->initializeScope();
  //for(int i = 0; i < Params->ListOfParams.size(); i++) {
  //  //instantiate one type for each variable
  //  std::unique_ptr<Type> UPType(Params->ListOfParams[i]->T->copy());
  //  //create variable with your type (primitive type and isArray {true or false})
  //  std::shared_ptr<VariableSymbol> SPVar = 
  //    std::make_shared<VariableSymbol>(std::move(UPType), Params->ListOfParams[i]->isArray);
  //  S->insert(Params->ListOfParams[i]->Name, std::move(SPVar));
  //  Args.push_back(Params->ListOfParams[i]->Name);
  //  //memory free
  //  if(i == Params->ListOfParams.size() - 1)
  //    delete Params->ListOfParams[i]->T;
  //}
  //delete Params;
  ////refresh log
  //LOG->scopes(S);
  ////create prototypeAST node
  ////return new PrototypeAST(Name, Args); 
}
  
Parameter* HandleParameter(const std::string &Name, bool isArray) {
  //Parameter *Param = new Parameter();
  //Param->Name = Name;
  //Param->isArray = isArray;
  //Param->Size = 0;
  //return Param;
}

Parameters* HandleParameters() {
  //return new Parameters();
}

void HandleParameters(Parameters* Params, Parameter* Param) {
  //std::unique_ptr<Parameter> UPParam(Param);
  //Params->ListOfParams.push_back(std::move(UPParam));
}

Type* HandleType(BasicType T, int Size) {
  //return new Type(T, Size);
}

Parameters* HandleListOfParams() {
  //return new Parameters();
}

void HandleListOfParams(Parameters *ParamsNew, Parameters *ParamsOld, Type *T) {
  //for(int i = 0; i < ParamsOld->ListOfParams.size(); i++) {
  //  ParamsOld->ListOfParams[i]->T = T;
  //}
  //while(ParamsOld->ListOfParams.size() > 0) {
  //  int N = ParamsOld->ListOfParams.size() - 1;
  //  ParamsNew->ListOfParams.push_back(std::move(ParamsOld->ListOfParams[N]));
  //  ParamsOld->ListOfParams.pop_back();
  //}
}

VarExprAST* HandleListOfVar() {
  //return new VarExprAST();
}

Var* HandleVar(const std::string &Name) {
  //return new Var(Name, nullptr, false);
}

Var* HandleVar(const std::string &Name, ExprAST* Expr) {
  //std::unique_ptr<ExprAST> UPExpr(Expr);
  //return new Var(Name, std::move(UPExpr), false);
}

void HandleListOfVar(VarExprAST *Vars, Var* Var) {
  //std::unique_ptr<Var> UPVar(Var);
  //Vars->addVar(std::move(UPVar));
}

void HandleVarCmd(VarExprAST* Vars, Type* T) {
  //std::unique_ptr<Type> UPT(T);
  //Vars->setType(std::move(UPT));
}

ProcedureAST* HandleProcedure(PrototypeAST* Proto, BlockAST* Block) {
  //finalize current scope
  //S->finalizeScope();
  //create and return ProcedureAST node
  //std::unique_ptr<PrototypeAST> UPProto(Proto);
  //std::unique_ptr<BlockAST> UPBlock(Block);
  //return new ProcedureAST(std::move(UPProto), std::move(UPBlock));
}

AssignAST* HandleAssign(const std::string Op, const std::string Name, ExprAST* Expr) {
  //std::shared_ptr<Symbol> Sym = S->findVariableSymbol(Name);
  //if(Sym == nullptr) {
  //  std::cout << "error\n";
  //  exit(1);
  //}else {
  //  std::unique_ptr<ExprAST> UPExpr(Expr);
  //  return new AssignAST(Op, Sym, std::move(UPExpr));
  //}
}

AssignAST* HandleAssign(const std::string Name, uint8_t Op, ExprAST* Expr) {
  //switch(Op) {
  //  case 1:
  //    return HandleAssign("+=", Name, Expr);
  //  case 2:
  //    return HandleAssign("-=", Name, Expr);
  //  case 3:
  //    return HandleAssign("*=", Name, Expr);
  //  case 4:
  //    return HandleAssign("/=", Name, Expr);
  //  default:
  //    return HandleAssign("%=", Name, Expr);
  //}
}

AssignAST* HandleAssign(uint8_t Op, const std::string Name) {
  //ExprAST* Expr = new NumberExprAST(1);
  //if(Op == 1) { //++ -> += 1
  //  return HandleAssign(Name, 1, Expr);
  //}else {       //-- -> -= 1
  //  return HandleAssign(Name, 2, Expr);
  //}
}
