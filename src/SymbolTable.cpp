#include "SymbolTable.hpp"

using namespace grc;

//===----------------------------------------------------------------------===//
//// Variable
//===----------------------------------------------------------------------===//

void VariableSymbol::toPrint(std::ofstream &File) {
  File << "Variable";
}

//===----------------------------------------------------------------------===//
//// Prototype 
//===----------------------------------------------------------------------===//

void ProcedureSymbol::toPrint(std::ofstream &File) {
  File << "Procedure";
}

//===----------------------------------------------------------------------===//
//// Function 
//===----------------------------------------------------------------------===//

void FunctionSymbol::toPrint(std::ofstream &File) {
  File << "Function";
}

//===----------------------------------------------------------------------===//
//// Type 
//===----------------------------------------------------------------------===//

Type* Type::copy() {
  return new Type(PrimitiveType, Size);
}

//===----------------------------------------------------------------------===//
//// Symbol Table
//===----------------------------------------------------------------------===//

bool SymbolTable::insert(const std::string &Name, std::unique_ptr<Symbol> Symbol) {
  if(!Table[Name]) {
    Table[Name] = std::move(Symbol);  
    return true;
  }
  return false;
}

void SymbolTable::toPrint(std::ofstream &File) {
  std::map<std::string,std::unique_ptr<Symbol>>::iterator it;
  for (it = Table.begin(); it != Table.end(); ++it) {
        File << "\t" << it->first << " => "; 
        it->second->toPrint(File);
        File << '\n';
  }
}
