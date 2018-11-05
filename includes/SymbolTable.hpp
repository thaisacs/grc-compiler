#pragma once

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
  };
 
  class VariableSymbol : public Symbol {
    std::unique_ptr<Type> T;
    bool isArray;
  public:
    VariableSymbol(std::unique_ptr<Type> T, bool isArray) : T(std::move(T)), isArray(isArray) {}
    SymbolType getType() override { return VARIABLE; }
    void toPrint(std::ofstream&) override;
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
    void toPrint(std::ofstream&);
    std::shared_ptr<Symbol> findVariableSymbol(const std::string&);
  };
}
