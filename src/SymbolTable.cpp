#include "SymbolTable.hpp"

using namespace grc;

//===----------------------------------------------------------------------===//
//// Variable
//===----------------------------------------------------------------------===//

std::shared_ptr<Type> VariableSymbol::getType() {
  return T;
}

SymbolType VariableSymbol::getSymbolType() {
  return SymbolType::Variable;
}

void VariableSymbol::toPrint(std::ofstream &File) {
  //File << "Variable (Type: ";
  //T->toPrint(File);
  //if(T->getPrimitiveType() == PrimitiveType::INT || 
  //   T->getPrimitiveType() == PrimitiveType::BOOL) {
  //  File << ", isArray: ";
  //  if(isArray)
  //    File << "True)";
  //  else
  //    File << "False)";
  //}else {
  //  File << ")";
  //}
}

//===----------------------------------------------------------------------===//
//// Procedure 
//===----------------------------------------------------------------------===//

std::shared_ptr<Type> ProcedureSymbol::getType() {
  return T;
}

SymbolType ProcedureSymbol::getSymbolType() {
  return SymbolType::Procedure;
}

void ProcedureSymbol::toPrint(std::ofstream &File) {
  //File << "Procedure";
}

//===----------------------------------------------------------------------===//
//// Function 
//===----------------------------------------------------------------------===//

std::shared_ptr<Type> FunctionSymbol::getType() {
  return T;
}

SymbolType FunctionSymbol::getSymbolType() {
  return SymbolType::Function;
}

void FunctionSymbol::toPrint(std::ofstream &File) {
  //File << "Function (Type: ";
  //T->toPrint(File);
  //File << ")";
}

//===----------------------------------------------------------------------===//
//// Type 
//===----------------------------------------------------------------------===//

void Type::toPrint(std::ofstream &File) {
  //switch(PT) {
  //  case 0:
  //    File << "INT";
  //    break;
  //  case 1:
  //    File << "BOOL";
  //    break;
  //  case 2:
  //    if(Size > 0)
  //      File << "STRING[" << Size << "]";
  //    else
  //      File << "STRING";
  //    break;
  //}
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
  //std::map<std::string, std::shared_ptr<Symbol>>::iterator it;
  //for (it = Table.begin(); it != Table.end(); ++it) {
  //  File << "\t" << it->first << " => "; 
  //  it->second->toPrint(File);
  //  File << '\n';
  //}
}
