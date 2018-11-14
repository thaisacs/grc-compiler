#include "Log.hpp"

using namespace grc;

void Log::info(const std::string &Msg) {
  File.open(FileName, std::ofstream::app);
  File << Msg << std::endl;
  File.close();
}

void Log::scopes(std::shared_ptr<Scope> S) {
  File.open(FileName, std::ofstream::app);
  S->toPrint(File);
  File.close();
}

void Log::subroutine(SubroutineAST *Proc) {
  File.open(FileName, std::ofstream::app);
  Proc->toPrint(File);
  File.close();
}

void Log::variable(VarExprAST *Vars) {
  File.open(FileName, std::ofstream::app);
  Vars->toPrint(File);
  File.close();
}
