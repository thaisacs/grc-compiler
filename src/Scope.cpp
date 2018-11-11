#include "Scope.hpp"

using namespace grc;

void Scope::finalizeScope() {
  SymbolTables.pop_back();
}
    
void Scope::initializeScope() {
  std::unique_ptr<grc::SymbolTable> ST = std::make_unique<grc::SymbolTable>();
  SymbolTables.push_back(std::move(ST));
}
    
bool Scope::insert(const std::string &Name, std::shared_ptr<Symbol> Symbol) {
  if(SymbolTables[SymbolTables.size() - 1]->insert(Name, Symbol))
    return true;
  return false;
}

std::shared_ptr<Symbol> Scope::findVariableSymbol(const std::string &Name) {
  for(int i = SymbolTables.size() - 1; i >= 0; i--) {
    auto Symb = SymbolTables[i]->findVariableSymbol(Name);
    if(Symb != nullptr)
      return Symb;
  }
  return nullptr;
}

void Scope::toPrint(std::ofstream &File) {
  File << "\t\t\t*******\n";
  for(int i = 0; i < SymbolTables.size(); i++) {
    File << "  -> Scope " << i << "\n";
    SymbolTables[i]->toPrint(File);
  }
  File << "\t\t\t*******\n";
}
