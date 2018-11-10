#pragma once

#include "llvm/IR/Value.h"

#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <fstream>

namespace grc {
  enum SymbolType {VARIABLE, PROCEDURE, FUNCTION};
  enum PrimitiveType {INT, BOOL, STRING};

  class Type {
    PrimitiveType PT;
    int Size;
    public:
      Type(PrimitiveType PT, int Size) : PT(PT), Size(Size) {}
      Type* copy();
      PrimitiveType getPrimitiveType() { return PT; }
      int getSize() { return Size; }
      void toPrint(std::ofstream&);
  };

  class Symbol {
  public:
    virtual ~Symbol() = default;
    virtual SymbolType getType() = 0;
    virtual void toPrint(std::ofstream&) = 0;
    virtual void setValue(llvm::Value*) {};
    virtual llvm::Value* getValue() {};
  };
 
  class VariableSymbol : public Symbol {
    std::unique_ptr<Type> T;
    bool isArray;
    llvm::Value* V;
  public:
    VariableSymbol(std::unique_ptr<Type> T, bool isArray) : 
      T(std::move(T)), isArray(isArray), V(nullptr) {}
    SymbolType getType() override { return VARIABLE; }
    void toPrint(std::ofstream&) override;
    llvm::Value* getValue() { return V; }
    void setValue(llvm::Value* VarValue) { V = VarValue;  }
  };

  class ProcedureSymbol : public Symbol {
  public:
    ProcedureSymbol() {}
    SymbolType getType() override { return PROCEDURE; }
    void toPrint(std::ofstream&) override;
  };

  class FunctionSymbol : public Symbol {
  public:
    FunctionSymbol() {}
    SymbolType getType() override { return FUNCTION; }
    void toPrint(std::ofstream&) override;
  }; 

  class SymbolTable {
    std::map<std::string, std::shared_ptr<Symbol>> Table; 
  public:    
    SymbolTable() {}
    bool insert(const std::string&, std::shared_ptr<Symbol>);
    std::shared_ptr<Symbol> findVariableSymbol(const std::string&);
    void setVariableValue(const std::string, llvm::Value*);
    llvm::Value* getVariableValue(const std::string&); 
    void toPrint(std::ofstream&);
  };
}
