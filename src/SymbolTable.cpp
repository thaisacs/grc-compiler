#include "SymbolTable.hpp"

using namespace grc;

//===----------------------------------------------------------------------===//
//// Type 
//===----------------------------------------------------------------------===//

void PrimitiveType::toPrint(std::ofstream &File) {
  switch(BT) {
    case BasicType::Int:
      File << "Int";
      break;
    case BasicType::Bool:
      File << "Bool";
      break;
    case BasicType::Void:
      File << "Void";
      break;
    case BasicType::Undefined:
      File << "Undefined";
      break;
    case BasicType::String:
      if(Size > 0)
        File << "String[" << Size << "]";
      else
        File << "String";
      break;
  }
}

void Type::toPrint(std::ofstream &File) {
  PType->toPrint(File);
  File << ", isArray: ";
  if(AType->isArray) {
    File << "True;";
    File << " Size: ";
    File << AType->Size;
  } else {
    File << "False";
  }
  //if(T->getPrimitiveType() == PrimitiveType::INT || 
  //   T->getPrimitiveType() == PrimitiveType::BOOL) {
  //  File << ", isArray: ";
  //  if(isArray)
  //    File << "True)";
  //  else
  //    File << "False)";
  //}else {
  //  File << ")";
  //}*/
}

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
  File << "Variable (Type: ";
  T->toPrint(File);
  File << ")";
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
  File << "Procedure (Type: ";
  T->toPrint(File);
  File << ")";
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
 File << "Function (Type: ";
 T->toPrint(File);
 File << ")";
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

std::shared_ptr<Symbol> SymbolTable::find(const std::string &Name) {
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
