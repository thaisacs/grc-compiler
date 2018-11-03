#include "Scope.hpp"

using namespace grc;


void Scope::finalizeScope() {
  SymbolTables.pop_back();
}
    
void Scope::initializeScope() {
  std::unique_ptr<grc::SymbolTable> ST = std::make_unique<grc::SymbolTable>();
  SymbolTables.push_back(std::move(ST));
}
    
bool Scope::insert(const std::string &Name, std::unique_ptr<Symbol> Symbol) {
  if(SymbolTables[SymbolTables.size() - 1]->insert(Name, std::move(Symbol)))
    return true;
  return false;
}

void Scope::toPrint() {
  for(int i = 0; i < SymbolTables.size(); i++) {
    std::cout << "Scope " << i << "\n";
    SymbolTables[i]->toPrint();
  }
}
