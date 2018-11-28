#include "Error.hpp"

using namespace grc;

extern bool isError;

void LogError(const std::string &Str) {
  if(Str != "")
    std::cerr << Str << std::endl;
  isError = true;
}

void LogError(const std::string &Str, unsigned line) {
  std::string ErrorMsg = "error [" + std::to_string(line) + "]: semantic error, " + Str;  
  std::cerr << ErrorMsg << std::endl;
  isError = true;
}

void LogWarning(const std::string &Str, unsigned line) {
  std::string WMsg = "warning [" + std::to_string(line) + "]: semantic error, " + Str;
  std::cerr << WMsg << std::endl;
  isError = true;
}
