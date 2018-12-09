#include "Scope.hpp"

using namespace grc;

void Scope::finalizeScope() {
  for(int i = SymbolTables.size() - 1; i > 0; i--)
    SymbolTables.pop_back();
  Current = 0;
}

void Scope::decrementSC() {
  --Current;
}
    
void Scope::incrementSC() {
  ++Current;
}

void Scope::initSC(int i) {
  Current = i;
}
    
void Scope::initializeScope() {
  std::unique_ptr<grc::SymbolTable> ST = std::make_unique<grc::SymbolTable>();
  SymbolTables.push_back(std::move(ST));
  ++Current;
}
    
bool Scope::insert(const std::string &Name, std::shared_ptr<Symbol> Symbol) {
  if(SymbolTables[Current]->insert(Name, Symbol))
    return true;
  return false;
}

std::shared_ptr<Symbol> Scope::find(const std::string &Name) {
  for(int i = Current; i >= 0; i--) {
    auto Symb = SymbolTables[i]->find(Name);
    if(Symb != nullptr) {
      return Symb;
    }
  }
  return nullptr;
}

std::shared_ptr<Symbol> Scope::findCurrentSub() {
  for(int i = Current; i >= 0; i--) {
    auto Symb = SymbolTables[i]->findCurrentSub();
    if(Symb != nullptr) {
      return Symb;
    }
  }
  return nullptr;
}

void Scope::toPrint(std::ofstream &File) {
  File << "///--------------------" << "\n";
  for(int i = 0; i < SymbolTables.size(); i++) {
    File << "Scope " << i << "\n";
    SymbolTables[i]->toPrint(File);
  }
  File << "///--------------------" << "\n";
}
