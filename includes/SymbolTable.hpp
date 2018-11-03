#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>

namespace grc {
  enum SymbolType {VARIABLE, PROCEDURE, FUNCTION};
  enum Types {UNDEFINED, INT, BOOL, STRING};

  class Type {
    Types PrimitiveType;
    int Size;
    public:
      Type(Types PT, int Size) : PrimitiveType(PT), Size(Size) {}
      void toPrint() {
        switch(PrimitiveType) {
          case 0:
            std::cout << "UNDEF ";
            break;
          case 1:
            std::cout << "INT ";
            break;
          case 2:
            std::cout << "BOOL ";
            break;
          case 3:
            std::cout << "STRING ";
            break;
        }
        std::cout << Size << std::endl;
      }
  };

  class Symbol {
    //std::string Name;
    //Type PrimitiveType;
  public:
    virtual ~Symbol() = default;
    virtual SymbolType getType() = 0;
    virtual void toPrint() = 0;
  };
 
  class VariableSymbol : public Symbol {
  public:
    VariableSymbol() {}
    SymbolType getType() override { return VARIABLE; }
    void toPrint() override { std::cout << "Variable"; };
  };

  class ProcedureSymbol : public Symbol {
  public:
    ProcedureSymbol() {}
    SymbolType getType() override { return PROCEDURE; }
    void toPrint() override { std::cout << "Procedure"; };
  };

  class FunctionSymbol : public Symbol {
  public:
    FunctionSymbol() {}
    SymbolType getType() override { return FUNCTION; }
    void toPrint() override { std::cout << "Function"; };
  }; 

  class SymbolTable {
    std::map<std::string, std::unique_ptr<Symbol>> Table; 
  public:    
    SymbolTable() {}
    bool insert(const std::string &Name, std::unique_ptr<Symbol> Symbol);
    void toPrint();
  };
}
