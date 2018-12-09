#pragma once

#include <vector>
#include <memory>
#include <fstream>

#include "SymbolTable.hpp"

namespace grc {
  class Scope {
    int Current;
    std::vector<std::unique_ptr<grc::SymbolTable>> SymbolTables;
  public:
    Scope() { Current = -1; }
    void initializeScope(); 
    void finalizeScope();
    bool insert(const std::string&, std::shared_ptr<Symbol>); 
    void decrementSC();
    void incrementSC();
    void initSC(int i);
    std::shared_ptr<Symbol> findCurrentSub();
    std::shared_ptr<Symbol> find(const std::string&); 
    void toPrint(std::ofstream&);
  };
}
