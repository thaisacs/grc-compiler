#include "Util.hpp"

#include "llvm/IR/Verifier.h"

using namespace grc;

extern std::shared_ptr<grc::Scope> S;
extern std::unique_ptr<grc::Log> LOG;
extern llvm::LLVMContext TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
extern int yylineno;

extern bool isError;

PrimitiveType* HandleType(BasicType T, int Size) {
  return new PrimitiveType(T, Size);
}

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
  //std::unique_ptr<ExprAST> UPCond(Cond);
  //std::unique_ptr<ExprAST> UPThen(Then);
  //std::unique_ptr<ExprAST> UPElse(Else);
  //return new IfExprAST(std::move(UPCond), std::move(UPThen), std::move(UPElse));
}

ExprAST* HandleCmdIf(ExprAST* Cond, ExprAST* Then) {
  //std::unique_ptr<ExprAST> UPCond(Cond);
  //std::unique_ptr<ExprAST> UPThen(Then);
  //return new IfExprAST(std::move(UPCond), std::move(UPThen), nullptr);
}


ExprAST* HandleCmdWhile(ExprAST* Cond, ExprAST* Block) {
  //std::unique_ptr<ExprAST> UPCond(Cond);
  //std::unique_ptr<ExprAST> UPBlock(Block);
  //return new WhileExprAST(std::move(UPCond), std::move(UPBlock));
}

Expressions* HandleCmdCall() {
  //return new Expressions();
}

void HandleCmdCall(Expressions *Exprs, ExprAST* Expr) {
  //std::unique_ptr<grc::ExprAST> UPExpr(Expr);
  //Exprs->ListOfExprs.push_back(std::move(UPExpr));
}

CallExprAST* HandleCmdCall(const std::string &Name, Expressions *Exprs) {
  //return new CallExprAST(Name, std::move(Exprs->ListOfExprs));
}

Expressions* HandleCmdWrite() {
  return new Expressions();
}

void HandleCmdWrite(Expressions *Exprs, ExprAST* Expr) {
  std::unique_ptr<grc::ExprAST> UPExpr(Expr);
  Exprs->ListOfExprs.push_back(std::move(UPExpr));
}

void HandleCmdWrite(Expressions *Exprs, const std::string &String) {
  auto StringAST = new StringExprAST(String);
  std::unique_ptr<grc::ExprAST> UPExpr(StringAST);
  Exprs->ListOfExprs.push_back(std::move(UPExpr));
}

WriteExprAST* HandleCmdWrite(Expressions *Exprs) {
  return new WriteExprAST(std::move(Exprs->ListOfExprs));
}







AssignAST* HandleAssign(const std::string &Op, const std::string &Name, ExprAST* Expr) {
  //std::shared_ptr<Symbol> Sym = S->findVariableSymbol(Name);
  //if(Sym == nullptr) {
  //  std::cout << "error\n";
  //  exit(1);
  //}else {
  //  std::unique_ptr<ExprAST> UPExpr(Expr);
  //  return new AssignAST(Op, Sym, std::move(UPExpr));
  //}
}

AssignAST* HandleAssign(const std::string &Name, uint8_t Op, ExprAST* Expr) {
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

AssignAST* HandleAssign(uint8_t Op, const std::string &Name) {
  //ExprAST* Expr = new NumberExprAST(1);
  //if(Op == 1) { //++ -> += 1
  //  return HandleAssign(Name, 1, Expr);
  //}else {       //-- -> -= 1
  //  return HandleAssign(Name, 2, Expr);
  //}
}

//===------------------------------------------------------------------------===//
//// Prototype and Subroutine 
////===----------------------------------------------------------------------===//

Parameter* HandleParameter(const std::string &Name, bool isArray) {
  Parameter *Param = new Parameter();
  Param->Name = Name;
  Param->AType = new ArrayType();
  Param->AType->isArray = isArray;
  Param->AType->Size = 0;
  return Param;
}

Parameters* HandleParameters() {
  return new Parameters();
}

void HandleParameters(Parameters* Params, Parameter* Param) {
  std::unique_ptr<Parameter> UPParam(Param);
  Params->ListOfParams.push_back(std::move(UPParam));
}

Parameters* HandleListOfParams() {
  return new Parameters();
}

void HandleListOfParams(Parameters *ParamsNew, Parameters *ParamsOld, PrimitiveType *T) {
  for(int i = 0; i < ParamsOld->ListOfParams.size(); i++) {
    ParamsOld->ListOfParams[i]->BT = T->getBasicType();
    ParamsOld->ListOfParams[i]->Size = T->getSize();
    if(T->getBasicType() == BasicType::String && T->getSize() == 0)
      ParamsOld->ListOfParams[i]->Size = 256;
  }
  while(ParamsOld->ListOfParams.size() > 0) {
    int N = ParamsOld->ListOfParams.size() - 1;
    ParamsNew->ListOfParams.push_back(std::move(ParamsOld->ListOfParams[N]));
    ParamsOld->ListOfParams.pop_back();
  }
}

SubroutineAST* HandleSubroutine(PrototypeAST* Proto, BlockExprAST* Block) {
  // create and return SubroutineAST node
  std::unique_ptr<PrototypeAST> UPProto(Proto);
  std::unique_ptr<BlockExprAST> UPBlock(Block);
  return new SubroutineAST(std::move(UPProto), std::move(UPBlock));
}

PrototypeAST* HandlePrototype(const std::string &Name, Parameters* Params) {
  std::vector<std::string> Args;
  // insert procedure in symbol table
  std::shared_ptr<PrimitiveType> SPPT = std::make_shared<PrimitiveType>(BasicType::Void, 0);
  std::shared_ptr<ArrayType> SPTArray(new ArrayType());
  SPTArray->isArray = false;
  SPTArray->Size = 0;
  std::shared_ptr<Type> SPT = std::make_shared<Type>(std::move(SPPT), std::move(SPTArray));
  std::unique_ptr<ProcedureSymbol> UPProc = std::make_unique<ProcedureSymbol>(SPT);
  
  
  S->insert(Name, std::move(UPProc));
  
  
  // start new scope
  S->initializeScope();
  // insert args in symbol table
  for(int i = 0; i < Params->ListOfParams.size(); i++) {
    std::shared_ptr<PrimitiveType> SPPT = std::make_shared<PrimitiveType>(Params->ListOfParams[i]->BT,
        Params->ListOfParams[i]->Size);
    std::shared_ptr<ArrayType> SPTArray(Params->ListOfParams[i]->AType);
    std::shared_ptr<Type> SPT = std::make_shared<Type>(std::move(SPPT), std::move(SPTArray));
    // create a variable 
    if(!S->find(Params->ListOfParams[i]->Name)) {
      std::shared_ptr<VariableSymbol> SPVar = std::make_shared<VariableSymbol>(SPT);
      S->insert(Params->ListOfParams[i]->Name, std::move(SPVar));
      Args.push_back(Params->ListOfParams[i]->Name);
    }else {
      std::string errorMsg = "error [" + std::to_string(yylineno) + "]: " + "redefinition of '" + 
        Params->ListOfParams[i]->Name + "'";
      HandleError(errorMsg);
    }
  }
  // create prototypeAST node
  return new PrototypeAST(Name, Args); 
}

PrototypeAST* HandlePrototype(const std::string &Name, Parameters* Params, PrimitiveType* T) {
  std::vector<std::string> Args;
  std::shared_ptr<PrimitiveType> SPPT(T);
  std::shared_ptr<ArrayType> SPTArray(new ArrayType());
  SPTArray->isArray = false;
  SPTArray->Size = 0;
  std::shared_ptr<Type> SPT = std::make_shared<Type>(std::move(SPPT), std::move(SPTArray));
  std::unique_ptr<FunctionSymbol> UPFunc = std::make_unique<FunctionSymbol>(std::move(SPT));
  
  
  S->insert(Name, std::move(UPFunc));
  
  
  // start new scope
  S->initializeScope();
  // insert args in symbol table
  for(int i = 0; i < Params->ListOfParams.size(); i++) {
    std::shared_ptr<PrimitiveType> SPPT = std::make_shared<PrimitiveType>(Params->ListOfParams[i]->BT,
        Params->ListOfParams[i]->Size);
    std::shared_ptr<ArrayType> SPTArray(Params->ListOfParams[i]->AType);
    std::shared_ptr<Type> SPT = std::make_shared<Type>(std::move(SPPT), std::move(SPTArray));
    // create a variable 
    if(!S->find(Params->ListOfParams[i]->Name)) {
      std::shared_ptr<VariableSymbol> SPVar = std::make_shared<VariableSymbol>(SPT);
      S->insert(Params->ListOfParams[i]->Name, std::move(SPVar));
      Args.push_back(Params->ListOfParams[i]->Name);
    }else {
      std::string errorMsg = "error [" + std::to_string(yylineno) + "]: " + "redefinition of '" + 
        Params->ListOfParams[i]->Name + "'";
      HandleError(errorMsg);
    }
  }
  // create prototypeAST node
  return new PrototypeAST(Name, Args); 
}

//===------------------------------------------------------------------------===//
//// Block
////===----------------------------------------------------------------------===//
 
BlockExprAST* HandleBlock(Expressions *Exprs) {
  return new BlockExprAST(std::move(Exprs->ListOfExprs));
}

Expressions* HandleCmd() {
  return new Expressions();
}

void HandleCmd(Expressions *Exprs, ExprAST *Expr) {
  std::unique_ptr<ExprAST> UPExpr(Expr);
  Exprs->ListOfExprs.push_back(std::move(UPExpr));
}

//===------------------------------------------------------------------------===//
//// Variable Declaration
////===----------------------------------------------------------------------===//

VarExprAST* HandleVarCmd(Variables *Vars, PrimitiveType *T) {
  // insert Variables in Symbol Table and generates new VarExprAST
  std::vector<std::unique_ptr<Var>> VecVars;
  std::shared_ptr<PrimitiveType> SPT(T);
  for(int i = 0; i < Vars->ListOfVars.size(); i++) {
    auto T = new Type(SPT, Vars->ListOfVars[i]->T);
    std::shared_ptr<Type> SPType(T);
    std::shared_ptr<VariableSymbol> SPVar = std::make_shared<VariableSymbol>(SPType);
    if(!S->find(Vars->ListOfVars[i]->V->getName())) {
      S->insert(Vars->ListOfVars[i]->V->getName(), SPVar);
      VecVars.push_back(std::move(Vars->ListOfVars[i]->V));
    }else {
      std::string errorMsg = "error [" + std::to_string(yylineno) + "]: " + "redefinition of '" + 
        Vars->ListOfVars[i]->V->getName() + "'";
      HandleError(errorMsg);
    }
  }
  return new VarExprAST(std::move(VecVars));
}

Variables* HandleListOfVar() {
  return new Variables();
}

void HandleListOfVar(Variables *Vars, VariableAndType *V) {
  std::unique_ptr<VariableAndType> UPVar(V);
  Vars->ListOfVars.push_back(std::move(UPVar));
}

// simple variable declarion without initialization
VariableAndType* HandleVar(const std::string &Name) {
  // config. Variable
  std::unique_ptr<Var> UPVar(new Var(Name, nullptr));
  // config. ArrayType
  std::shared_ptr<ArrayType> SPAType = std::make_shared<ArrayType>();
  SPAType->isArray = false;
  SPAType->Size = 0;
  // save Variable and Type together
  auto VT = new VariableAndType();
  VT->V = std::move(UPVar);
  VT->T = std::move(SPAType);
  return VT;
}

// simple variable declarion with initialization
VariableAndType* HandleVar(const std::string &Name, ExprAST *Expr) {
  std::unique_ptr<ExprAST> UPExpr(Expr);
  // config. Variable
  std::unique_ptr<Var> UPVar(new Var(Name, std::move(UPExpr)));
  // config. ArrayType
  std::shared_ptr<ArrayType> SPAType = std::make_shared<ArrayType>();
  SPAType->isArray = false;
  SPAType->Size = 0;
  // save Variable and Type together
  auto VT = new VariableAndType();
  VT->V = std::move(UPVar);
  VT->T = std::move(SPAType);
  return VT;
}

// array variable
VariableAndType* HandleVar(const std::string &Name,  int Size, ExprAST *Expr) {
  std::unique_ptr<ExprAST> UPExpr(Expr);
  // config. Variable
  std::unique_ptr<Var> UPVar(new Var(Name, std::move(UPExpr)));
  // config. ArrayType
  std::shared_ptr<ArrayType> SPAType = std::make_shared<ArrayType>();
  SPAType->isArray = true;
  SPAType->Size = Size;
  // save Variable and Type together
  auto VT = new VariableAndType();
  VT->V = std::move(UPVar);
  VT->T = std::move(SPAType);
  return VT;
}

// string variable
VariableAndType* HandleVar(const std::string &Name, const std::string &String) {
  StringExprAST *Expr = new StringExprAST(String);
  std::unique_ptr<ExprAST> UPExpr(Expr);
  // config. Variable
  std::unique_ptr<Var> UPVar(new Var(Name, std::move(UPExpr)));
  // config. ArrayType
  std::shared_ptr<ArrayType> SPAType = std::make_shared<ArrayType>();
  SPAType->isArray = false;
  SPAType->Size = 0;
  // save Variable and Type together
  auto VT = new VariableAndType();
  VT->V = std::move(UPVar);
  VT->T = std::move(SPAType);
  return VT;
}

Booleans* HandleListOfBool() {
  return new Booleans();
}

void HandleBool(Booleans *Bools, bool b) {
  Bools->ListOfBools.push_back(b);
}

BooleansExprAST* HandleBool(Booleans *Bools) {
  return new BooleansExprAST(std::move(Bools->ListOfBools));
}

Integers* HandleListOfInt() {
  return new Integers();
}

void HandleInt(Integers *Ints, int i) {
  Ints->ListOfInts.push_back(i);
}

IntegersExprAST* HandleInt(Integers *Ints) {
  return new IntegersExprAST(std::move(Ints->ListOfInts));
}

//===------------------------------------------------------------------------===//
//// Errors 
////===----------------------------------------------------------------------===//

void HandleError(const std::string &Msg) {
  LogError(Msg);
  isError = true;
}

void VerifyMain() {
    if(!S->find("main")) {
      HandleError("error [all]: program there is no main() function");
    }
}

//===------------------------------------------------------------------------===//
//// Import 
////===----------------------------------------------------------------------===//

void HandleImportIO() {
  // add printf
  TheModule->getOrInsertFunction("printf",
            llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(TheContext), 
            llvm::PointerType::get(llvm::Type::getInt8Ty(TheContext), 0), true));
  // add scanf
  TheModule->getOrInsertFunction("scanf",
            llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(TheContext), 
            llvm::PointerType::get(llvm::Type::getInt8Ty(TheContext), 0), true));
}
