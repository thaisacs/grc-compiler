#pragma once

#include <string>
#include <fstream>
#include <memory>

#include "Scope.hpp"
#include "AST.hpp"
#include "Util.hpp"

namespace grc {
  class Log {
    std::string FileName;
    std::ofstream File;
  public:
    Log(const std::string FileName) : FileName(FileName) {
      File.open(FileName);
      File << "//===----------------------------------------------------------------------===//\n";
      File << "//// grc-compiler log\n";
      File << "//===----------------------------------------------------------------------===//\n";
      File.close();
    }
    void scopes(std::shared_ptr<Scope>); 
    void info(const std::string&);
  };
}
