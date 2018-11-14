#pragma once

#include <vector>
#include <memory>
#include <fstream>

#include "SymbolTable.hpp"

namespace grc {
  class Scope {
    std::vector<std::unique_ptr<grc::SymbolTable>> SymbolTables;
  public:
    Scope() {}
    void initializeScope(); 
    void finalizeScope();
    bool insert(const std::string&, std::shared_ptr<Symbol>); 
    std::shared_ptr<Symbol> find(const std::string&); 
    void toPrint(std::ofstream&);
  };
}
