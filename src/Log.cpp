#include "Log.hpp"

using namespace grc;

void Log::scopes(std::shared_ptr<Scope> S) {
  File.open(FileName, std::ofstream::app);
  S->toPrint(File);
  File.close();
}

void Log::procedure(ProcedureAST* Proc) {
  File.open(FileName, std::ofstream::app);
  Proc->toPrint(File);
  File.close();
}

