#include "SymbolTable.hpp"

using namespace grc;

bool SymbolTable::insert(const std::string &Name, std::unique_ptr<Symbol> Symbol) {
  if(!Table[Name]) {
    Table[Name] = std::move(Symbol);  
    return true;
  }
  return false;
}

void SymbolTable::toPrint() {
  std::map<std::string,std::unique_ptr<Symbol>>::iterator it;
  for (it = Table.begin(); it != Table.end(); ++it) {
        std::cout << it->first << " => "; 
        it->second->toPrint();
        std::cout << '\n';
  }
}
