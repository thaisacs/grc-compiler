#pragma once

#include <iostream>
#include <vector>

namespace grc {
  class Symbol {
    std::string Name;
  public:
  };
  class SymbolTable {
    std::vector<Symbol> table; 
  public:    
    SymbolTable();
  };
}
