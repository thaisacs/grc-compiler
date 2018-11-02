#pragma once

#include <iostream>
#include <vector>

namespace compiler {
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
