#include "Log.hpp"

using namespace grc;

void Log::scopes(std::shared_ptr<Scope> S) {
  File.open(FileName, std::ofstream::app);
  S->toPrint(File);
  File.close();
}
