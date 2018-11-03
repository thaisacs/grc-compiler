#pragma once

#include <vector>
#include <memory>

#include "SymbolTable.hpp"

namespace grc {
  class Scope {
    std::vector<std::unique_ptr<grc::SymbolTable>> SymbolTables;
  public:
    Scope() {}
    void initializeScope(); 
    bool insert(const std::string &Name, std::unique_ptr<Symbol> Symbol); 
    void finalizeScope();
    void toPrint();
  };
}
