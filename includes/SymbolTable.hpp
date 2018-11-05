#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <fstream>

namespace grc {
  enum SymbolType {VARIABLE, PROCEDURE, FUNCTION};
  enum Types {INT, BOOL, STRING};

  class Type {
    Types PrimitiveType;
    int Size;
    public:
      Type(Types PT, int Size) : PrimitiveType(PT), Size(Size) {}
      Type* copy();
      /*
      void toPrint() {
        switch(PrimitiveType) {
          case 0:
            std::cout << "INT ";
            break;
          case 1:
            std::cout << "BOOL ";
            break;
          case 2:
            std::cout << "STRING ";
            break;
        }
        std::cout << Size << std::endl;
      }*/
  };

  class Symbol {
  public:
    virtual ~Symbol() = default;
    virtual SymbolType getType() = 0;
    virtual void toPrint(std::ofstream&) = 0;
  };
 
  class VariableSymbol : public Symbol {
    std::unique_ptr<Type> T;
    //bool isArray;
  public:
    VariableSymbol(std::unique_ptr<Type> T) : T(std::move(T)) {}
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
    std::map<std::string, std::unique_ptr<Symbol>> Table; 
  public:    
    SymbolTable() {}
    bool insert(const std::string &Name, std::unique_ptr<Symbol> Symbol);
    void toPrint(std::ofstream&);
  };
}
