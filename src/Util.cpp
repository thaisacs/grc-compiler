#include "Util.hpp"

#include "llvm/IR/Verifier.h"

using namespace grc;

extern std::shared_ptr<grc::Scope> S;
extern std::unique_ptr<grc::Log> LOG;
extern llvm::LLVMContext TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
extern int yylineno;

//===------------------------------------------------------------------------===//
//// Type 
////===----------------------------------------------------------------------===//

PrimitiveType* HandleType(BasicType T, int Size) {
  return new PrimitiveType(T, Size);
}

//===------------------------------------------------------------------------===//
//// Variable 
////===----------------------------------------------------------------------===//

grc::VariableExprAST* HandleVariable(const std::string &Name) {
  return new VariableExprAST(Name, nullptr);
}

grc::VariableExprAST* HandleVariable(const std::string &Name, grc::ExprAST *Expr) {
  std::unique_ptr<ExprAST> UPExpr(Expr);
  return new VariableExprAST(Name, std::move(UPExpr));
}

//===------------------------------------------------------------------------===//
//// Expression 
////===----------------------------------------------------------------------===//

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
      return new BinaryExprAST(">=", std::move(UPExprL), std::move(UPExprR));
      break;
    default:
      return new BinaryExprAST("<=", std::move(UPExprL), std::move(UPExprR));
  }
}

//===------------------------------------------------------------------------===//
//// Cmd: Return 
////===----------------------------------------------------------------------===//

ExprAST* HandleCmdReturn(ExprAST *Expr) {
  std::unique_ptr<ExprAST> UPVal(Expr);
  return new ReturnExprAST(std::move(UPVal));
}

//===------------------------------------------------------------------------===//
//// Cmd: If 
////===----------------------------------------------------------------------===//

ExprAST* HandleCmdIf(ExprAST* Cond, ExprAST* Then, ExprAST* Else) {
  if(Cond->getResultingType() != BasicType::Bool) {
    std::string ErrorMsg = "if condition is not of type Bool";
    LogError(ErrorMsg, yylineno);
    return nullptr; 
  }
  std::unique_ptr<ExprAST> UPCond(Cond);
  std::unique_ptr<ExprAST> UPThen(Then);
  std::unique_ptr<ExprAST> UPElse(Else);
  return new IfExprAST(std::move(UPCond), std::move(UPThen), std::move(UPElse));
}

ExprAST* HandleCmdIf(ExprAST* Cond, ExprAST* Then) {
  if(Cond->getResultingType() != BasicType::Bool) {
    std::string ErrorMsg = "if condition is not of type Bool";
    LogError(ErrorMsg, yylineno);
    return nullptr; 
  }
  std::unique_ptr<ExprAST> UPCond(Cond);
  std::unique_ptr<ExprAST> UPThen(Then);
  return new IfExprAST(std::move(UPCond), std::move(UPThen), nullptr);
}

//===------------------------------------------------------------------------===//
//// Cmd: For 
////===----------------------------------------------------------------------===//

ExprAST* HandleCmdFor(ExprAST *Start, ExprAST *End, ExprAST *Step, ExprAST *Body) {
  std::unique_ptr<ExprAST> UPStart(Start);
  std::unique_ptr<ExprAST> UPEnd(End);
  std::unique_ptr<ExprAST> UPStep(Step);
  std::unique_ptr<ExprAST> UPBody(Body);
  return new ForExprAST(std::move(UPStart), std::move(UPEnd), std::move(UPStep),
      std::move(UPBody));
}

//===------------------------------------------------------------------------===//
//// Cmd: While 
////===----------------------------------------------------------------------===//

ExprAST* HandleCmdWhile(ExprAST* Cond, ExprAST* Block) {
  if(Cond->getResultingType() != BasicType::Bool) {
    std::string ErrorMsg = "while condition is not of type Bool";
    LogError(ErrorMsg, yylineno);
    return nullptr; 
  }
  std::unique_ptr<ExprAST> UPCond(Cond);
  std::unique_ptr<ExprAST> UPBlock(Block);
  return new WhileExprAST(std::move(UPCond), std::move(UPBlock));
}

//===------------------------------------------------------------------------===//
//// Cmd: Called 
////===----------------------------------------------------------------------===//

Expressions* HandleCmdCall() {
  return new Expressions();
}

void HandleCmdCall(Expressions *Exprs, ExprAST* Expr) {
  std::unique_ptr<grc::ExprAST> UPExpr(Expr);
  Exprs->ListOfExprs.push_back(std::move(UPExpr));
}

CallExprAST* HandleCmdCall(const std::string &Name, Expressions *Exprs) {
  Symbol *SubSymbol = (S->find(Name)).get();
 
  //if(SubSymbol->getSymbolType() == SymbolType::Procedure) {
  //  ProcedureSymbol *Proc = static_cast<ProcedureSymbol*>(SubSymbol);
  //  if(Exprs->ListOfExprs.size() != Proc->getSizeArgs()) {
  //    std::string ErrorMsg = "invalid arguments in procedure call '" 
  //      + Name + "'";
  //    LogError(ErrorMsg, yylineno);
  //    return nullptr;
  //  }
  //  for(unsigned i = 0; i < Exprs->ListOfExprs.size(); i++) {
  //    auto TArg = Proc->getTypeArg(i);
  //    auto TExpr = Exprs->ListOfExprs[i]->getResultingType();
  //    if(TArg->getPrimitiveType()->getBasicType() != TExpr) {
  //      std::string ErrorMsg = "invalid arguments in procedure call '" 
  //        + Name + "'";
  //      LogError(ErrorMsg, yylineno);
  //      return nullptr;
  //    }
  //  }
  //}else if(SubSymbol->getSymbolType() == SymbolType::Function) {
  //  FunctionSymbol *Func = static_cast<FunctionSymbol*>(SubSymbol);
  //  if(Exprs->ListOfExprs.size() != Func->getSizeArgs()) {
  //    std::string ErrorMsg = "invalid arguments in function call '" 
  //      + Name + "'";
  //    LogError(ErrorMsg, yylineno);
  //    return nullptr;
  //  }
  //  for(unsigned i = 0; i < Exprs->ListOfExprs.size(); i++) {
  //    auto TArg = Func->getTypeArg(i);
  //    auto TExpr = Exprs->ListOfExprs[i]->getResultingType();
  //    if(TArg->getPrimitiveType()->getBasicType() != TExpr) {
  //      std::string ErrorMsg = "invalid arguments in function call '" 
  //        + Name + "'";
  //      LogError(ErrorMsg, yylineno);
  //      return nullptr;
  //    }
  //  }
  //}else {
  //  std::string ErrorMsg = "'" + Name + "' is not a subroutine";
  //  LogError(ErrorMsg, yylineno);
  //  return nullptr;
  //}
  return new CallExprAST(Name, std::move(Exprs->ListOfExprs));
}

//===------------------------------------------------------------------------===//
//// Cmd: Write and Read 
////===----------------------------------------------------------------------===//

Expressions* HandleCmdWrite() {
  return new Expressions();
}

void HandleCmdWrite(Expressions *Exprs, ExprAST* Expr) {
  std::unique_ptr<grc::ExprAST> UPExpr(Expr);
  Exprs->ListOfExprs.push_back(std::move(UPExpr));
}

void HandleCmdWrite(Expressions *Exprs, const std::string &String) {
  auto StringAST = new StringExprAST(String);
  std::unique_ptr<ExprAST> UPExpr(StringAST);
  Exprs->ListOfExprs.push_back(std::move(UPExpr));
}

WriteExprAST* HandleCmdWrite(Expressions *Exprs) {
  return new WriteExprAST(std::move(Exprs->ListOfExprs));
}

ReadExprAST* HandleCmdRead(const std::string &Iden) {
  std::unique_ptr<ExprAST> UPExpr(nullptr);
  return new ReadExprAST(Iden, std::move(UPExpr));
}

ReadExprAST* HandleCmdRead(const std::string &Iden, grc::ExprAST *Expr) {
  std::unique_ptr<ExprAST> UPExpr(Expr);
  return new ReadExprAST(Iden, std::move(UPExpr));
} 

//===------------------------------------------------------------------------===//
//// Cmd: Assign 
////===----------------------------------------------------------------------===//

AssignExprAST* HandleAssign(const std::string &Op, VariableExprAST *Var, ExprAST* Expr) {
  std::string Name = Var->getName(); 
  std::shared_ptr<Symbol> Sym = S->find(Name);
  if(Sym == nullptr) {
    std::string MsgError = "variable '" + Name + "' was not declared";
    LogError(MsgError, yylineno);
    return nullptr;
  }else {
    if(Sym->getSymbolType() != SymbolType::Variable) {
      std::string MsgError = "'" + Name + "' is not a variable to receive assignment";
      LogError(MsgError, yylineno);
      return nullptr;
    } 
    auto ExprBT = Expr->getResultingType();
    auto VarBT = Var->getResultingType();

  //  if(TBT == BasicType::Int) {
  //    if(TAT->isArray) {
  //      TBT = BasicType::IntArray;
  //    }
  //  }else if(TBT == BasicType::Bool) {
  //    if(TAT->isArray) {
  //      TBT = BasicType::BoolArray;
  //    }
  //  }

    if(ExprBT == VarBT) {   
      std::unique_ptr<VariableExprAST> UPVar(Var);
      std::unique_ptr<ExprAST> UPExpr(Expr);
      return new AssignExprAST(Op, std::move(UPVar), std::move(UPExpr));
    }else {
        std::string ErrorMsg = "the type of expression and variable '" 
          + Name + "' are different";
        LogError(ErrorMsg, yylineno);
        return nullptr;
    } 
  }
}

AssignExprAST* HandleAssign(VariableExprAST *Var, uint8_t Op, ExprAST* Expr) {
  switch(Op) {
    case 1:
      return HandleAssign("+=", Var, Expr);
    case 2:
      return HandleAssign("-=", Var, Expr);
    case 3:
      return HandleAssign("*=", Var, Expr);
    case 4:
      return HandleAssign("/=", Var, Expr);
    default:
      return HandleAssign("%=", Var, Expr);
  }
  return nullptr;
}

AssignExprAST* HandleAssign(uint8_t Op, VariableExprAST *Var) {
  ExprAST* Expr = new NumberExprAST(1);
  if(Op == 1) { //++ -> += 1
    return HandleAssign(Var, 1, Expr);
  }else {       //-- -> -= 1
    return HandleAssign(Var, 2, Expr);
  }
  return nullptr;
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

void HandleListOfParams(Parameters *ParamsNew, Parameters *ParamsOld) {
  while(ParamsOld->ListOfParams.size() > 0) {
    int N = ParamsOld->ListOfParams.size() - 1;
    ParamsNew->ListOfParams.push_back(std::move(ParamsOld->ListOfParams[N]));
    ParamsOld->ListOfParams.pop_back();
  }
}

void HandleListOfParams(Parameters *Params, PrimitiveType *T) {
  for(int i = 0; i < Params->ListOfParams.size(); i++) {
    Params->ListOfParams[i]->BT = T->getBasicType();
    Params->ListOfParams[i]->Size = T->getSize();
    //if(T->getBasicType() == BasicType::String && T->getSize() == 0)
    //  ParamsOld->ListOfParams[i]->Size = 255;
  }
}

SubroutineAST* HandleSubroutine(PrototypeAST *Proto, BlockExprAST *Block) {
  // create and return SubroutineAST node
  std::unique_ptr<PrototypeAST> UPProto(Proto);
  std::unique_ptr<BlockExprAST> UPBlock(Block);
  return new SubroutineAST(std::move(UPProto), std::move(UPBlock));
}

PrototypeAST* HandlePrototype(const std::string &Name, Parameters* Params) {
  std::vector<std::string> Args;
  std::vector<std::shared_ptr<Type>> TArgs;
  
  for(int i = 0; i < Params->ListOfParams.size(); i++) {
    auto Param = Params->ListOfParams[i].get();
    std::shared_ptr<PrimitiveType> SPPT = std::make_shared<PrimitiveType>(Param->BT,
        Param->Size);
    std::shared_ptr<ArrayType> SPTArray(Param->AType);
    std::shared_ptr<Type> SPT = 
      std::make_shared<Type>(std::move(SPPT), std::move(SPTArray));
    
    TArgs.push_back(SPT);
  }
  std::shared_ptr<PrimitiveType> SPPT = 
    std::make_shared<PrimitiveType>(BasicType::Void, 0);
  std::shared_ptr<ArrayType> SPTArray(new ArrayType());
  SPTArray->isArray = false;
  SPTArray->Size = 0;
  std::shared_ptr<Type> SPT = 
    std::make_shared<Type>(std::move(SPPT), std::move(SPTArray));

  std::unique_ptr<ProcedureSymbol> UPProc = 
    std::make_unique<ProcedureSymbol>(std::move(SPT), TArgs);
 
  if(!S->insert(Name, std::move(UPProc))) {
    std::string MsgError = "'" + Name + "' function already exists in the current scope";
    LogError(MsgError, yylineno);
  }
  
  // start new scope
  S->initializeScope();
  
  // insert args in symbol table
  for(int i = 0; i < Params->ListOfParams.size(); i++) {
    std::shared_ptr<VariableSymbol> SPVar = std::make_shared<VariableSymbol>(TArgs[i], true);
    // create a variable 
    if(S->insert(Params->ListOfParams[i]->Name, std::move(SPVar))) {
      Args.push_back(Params->ListOfParams[i]->Name);
    }else {
      std::string ErrorMsg = "redeclaration of '" + Params->ListOfParams[i]->Name + "'";
      LogError(ErrorMsg, yylineno);
    }
  }

  // create prototypeAST node
  return new PrototypeAST(Name, Args); 
}

PrototypeAST* HandlePrototype(const std::string &Name, Parameters* Params, PrimitiveType* T) {
  std::vector<std::string> Args;
  std::vector<std::shared_ptr<Type>> TArgs;
  
  for(int i = 0; i < Params->ListOfParams.size(); ++i) {
    auto Param = Params->ListOfParams[i].get();
    std::shared_ptr<PrimitiveType> SPPT = std::make_shared<PrimitiveType>(Param->BT,
        Param->Size);
    std::shared_ptr<ArrayType> SPTArray(Param->AType);
    std::shared_ptr<Type> SPT = 
      std::make_shared<Type>(std::move(SPPT), std::move(SPTArray));
    
    TArgs.push_back(SPT);
  }
  
  std::shared_ptr<PrimitiveType> SPPT(T);
  std::shared_ptr<ArrayType> SPTArray(new ArrayType());
  SPTArray->isArray = false;
  SPTArray->Size = 0;
  std::shared_ptr<Type> SPT = 
    std::make_shared<Type>(std::move(SPPT), std::move(SPTArray));
  std::unique_ptr<FunctionSymbol> UPFunc = 
    std::make_unique<FunctionSymbol>(std::move(SPT), TArgs);
 
  if(!S->insert(Name, std::move(UPFunc))) {
    std::string MsgError = "'" + Name + "' function already exists in the current scope";
    LogError(MsgError, yylineno);
  }
  
  // start new scope
  S->initializeScope();
  
  // insert args in symbol table
  for(int i = Params->ListOfParams.size()-1; i >= 0; i--) {
    std::shared_ptr<VariableSymbol> SPVar = std::make_shared<VariableSymbol>(TArgs[i], true);
    // create a variable 
    if(S->insert(Params->ListOfParams[i]->Name, std::move(SPVar))) {
      Args.push_back(Params->ListOfParams[i]->Name);
    }else {
      std::string ErrorMsg = "redeclaration of '" + Params->ListOfParams[i]->Name + "'";
      LogError(ErrorMsg, yylineno);
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
  if(Expr != nullptr) {
    std::unique_ptr<ExprAST> UPExpr(Expr);
    Exprs->ListOfExprs.push_back(std::move(UPExpr));
  }
}

//===------------------------------------------------------------------------===//
//// Variable Declaration
////===----------------------------------------------------------------------===//

VarExprAST* HandleVarCmd(Variables *Vars, PrimitiveType *T) {
  // insert Variables in Symbol Table
  std::vector<std::unique_ptr<AssignExprAST>> VecVars;
  std::shared_ptr<PrimitiveType> SPT(T);
  for(unsigned i = 0; i < Vars->ListOfVars.size(); i++) {
    auto Assign = Vars->ListOfVars[i]->V.get();
    auto Var = Assign->getVar();
    auto Expr = Assign->getExpr();
    if(Expr != nullptr) {
      auto ExprBT = Expr->getResultingType();
      auto TBT = T->getBasicType();

      if(TBT == BasicType::Int) {
        if(Vars->ListOfVars[i]->T->isArray) {
          TBT = BasicType::IntArray;
        }
      }else if(TBT == BasicType::Bool) {
        if(Vars->ListOfVars[i]->T->isArray) {
          TBT = BasicType::BoolArray;
        }
      }

      if(ExprBT == TBT) { //dec. type and expr. type equals
        //config. type var i
        auto T = new Type(SPT, Vars->ListOfVars[i]->T);
        std::shared_ptr<Type> SPType(T);
        std::shared_ptr<VariableSymbol> SPVar = std::make_shared<VariableSymbol>(SPType, false);
        //insert var i in Symbol Table
        if(S->insert(Var->getName(), SPVar)) {
          VecVars.push_back(std::move(Vars->ListOfVars[i]->V));
        }else {
          std::string ErrorMsg = "redeclaration of '" + Var->getName() + "'";
          LogError(ErrorMsg, yylineno);
        }
      }else {
        std::string ErrorMsg = "type of declaration and assignment of variable  '" 
          + Var->getName() + "' are different";
        LogError(ErrorMsg, yylineno);
      }
    }else {
      //config. type var i
      auto T = new Type(SPT, Vars->ListOfVars[i]->T);
      std::shared_ptr<Type> SPType(T);
      std::shared_ptr<VariableSymbol> SPVar = std::make_shared<VariableSymbol>(SPType, false);
      //insert var i in Symbol Table
      if(S->insert(Var->getName(), SPVar)) {
        VecVars.push_back(std::move(Vars->ListOfVars[i]->V));
      }else {
        std::string ErrorMsg = "redeclaration of '" + Var->getName() + "'";
        LogError(ErrorMsg, yylineno);
      }
    }
  }
  //// generates new VarExprAST
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
  std::unique_ptr<VariableExprAST> UPVar(new VariableExprAST(Name, nullptr));
  std::unique_ptr<AssignExprAST> UPAssign(new AssignExprAST("=", std::move(UPVar), nullptr));
  // config. ArrayType
  std::shared_ptr<ArrayType> SPAType = std::make_shared<ArrayType>();
  SPAType->isArray = false;
  SPAType->Size = 0;
  // save Variable and Type together
  auto VT = new VariableAndType();
  VT->V = std::move(UPAssign);
  VT->T = std::move(SPAType);
  return VT;
}

// array variable declarion without initialization
VariableAndType* HandleVar(const std::string &Name, int Size) {
  // config. Variable
  std::unique_ptr<VariableExprAST> UPVar(new VariableExprAST(Name, nullptr));
  std::unique_ptr<AssignExprAST> UPAssign(new AssignExprAST("=", std::move(UPVar), nullptr));
  // config. ArrayType
  std::shared_ptr<ArrayType> SPAType = std::make_shared<ArrayType>();
  SPAType->isArray = true;
  SPAType->Size = Size;
  // save Variable and Type together
  auto VT = new VariableAndType();
  VT->V = std::move(UPAssign);
  VT->T = std::move(SPAType);
  return VT;
}

// simple variable declarion with initialization
VariableAndType* HandleVar(const std::string Op, const std::string &Name, ExprAST *Expr) {
  std::unique_ptr<ExprAST> UPExpr(Expr);
  // config. Variable
  std::unique_ptr<VariableExprAST> UPVarable(new VariableExprAST(Name, nullptr));
  std::unique_ptr<AssignExprAST> UPVar(new AssignExprAST(Op, 
        std::move(UPVarable), std::move(UPExpr)));
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

// array variable declarion with initialization
VariableAndType* HandleVar(const std::string &Name, int Size, ExprAST *Expr) {
  std::unique_ptr<ExprAST> UPExpr(Expr);
  // config. Variable
  std::unique_ptr<VariableExprAST> UPVarable(new VariableExprAST(Name, nullptr));
  std::unique_ptr<AssignExprAST> UPVar(new AssignExprAST("=", 
        std::move(UPVarable), std::move(UPExpr)));
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
  std::unique_ptr<VariableExprAST> UPVariable(new VariableExprAST(Name, nullptr));
  std::unique_ptr<AssignExprAST> UPVar(new AssignExprAST("=", 
        std::move(UPVariable), std::move(UPExpr)));
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

void VerifyMain() {
  std::shared_ptr<Symbol> Main = S->find("main");
  if(!Main) {
    LogError("error: 'main' function not found");
  }else {
    if(Main->getType()->getPrimitiveType()->getBasicType() != BasicType::Int) {
      LogError("error: 'main' function must be int type");
    } 
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

//===------------------------------------------------------------------------===//
//// Stop Or Skip
////===----------------------------------------------------------------------===//

/*
StopOrSkipExprAST* HandleCmdStopOrSkip(BasicItCmd Type) {
  std::string ErrorMsg = "faltal error: grcc does not yet support stop and skip";
  LogError(ErrorMsg); 
  exit(1);
}
*/
