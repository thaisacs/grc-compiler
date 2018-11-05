#include "SymbolTable.hpp"

using namespace grc;

//===----------------------------------------------------------------------===//
//// Variable
//===----------------------------------------------------------------------===//

void VariableSymbol::toPrint(std::ofstream &File) {
  File << "Variable (Type: ";
  T->toPrint(File);
  if(T->getPrimitiveType() == PrimitiveType::INT) {
    File << ", isArray: ";
    if(isArray)
      File << "True)";
    else
      File << "False)";
  }else {
    File << ")";
  }
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
  return new Type(PT, Size);
}

void Type::toPrint(std::ofstream &File) {
  switch(PT) {
    case 0:
      File << "INT";
      break;
    case 1:
      File << "BOOL";
      break;
    case 2:
      if(Size > 0)
        File << "STRING[" << Size << "]";
      else
        File << "STRING";
      break;
  }
}

//===----------------------------------------------------------------------===//
//// Symbol Table
//===----------------------------------------------------------------------===//

bool SymbolTable::insert(const std::string &Name, std::shared_ptr<Symbol> Symbol) {
  if(!Table[Name]) {
    Table[Name] = Symbol;  
    return true;
  }
  return false;
}

std::shared_ptr<Symbol> SymbolTable::findVariableSymbol(const std::string &Name) {
  std::map<std::string, std::shared_ptr<Symbol>>::iterator it;
  for(it = Table.begin(); it != Table.end(); ++it) {
    if(it->first == Name) {
      return it->second;
    }
  }
  return nullptr;
}

void SymbolTable::toPrint(std::ofstream &File) {
  std::map<std::string, std::shared_ptr<Symbol>>::iterator it;
  for (it = Table.begin(); it != Table.end(); ++it) {
        File << "\t" << it->first << " => "; 
        it->second->toPrint(File);
        File << '\n';
  }
}
