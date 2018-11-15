#pragma once

#include "llvm/IR/Value.h"

#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <fstream>

namespace grc {
  enum SymbolType {Variable, Procedure, Function};
  enum BasicType {Int, Bool, String, Void};

  struct ArrayType {
    bool isArray;
    int Size;
  };

  class PrimitiveType {
    BasicType BT;
    int Size;
    public:
      PrimitiveType(BasicType BT, int Size) : BT(BT), Size(Size) {}
      BasicType getBasicType() { return BT; }
      int getSize() { return Size; }
      void toPrint(std::ofstream&);
  };

  class Type {
    std::shared_ptr<PrimitiveType> PType;
    std::shared_ptr<ArrayType> AType;
  public:
    Type(std::shared_ptr<PrimitiveType> PT, std::shared_ptr<ArrayType> AT) : PType(PT), AType(AT) { }
    void setPrimitiveType(std::shared_ptr<PrimitiveType> P) { PType = P; }
    std::shared_ptr<PrimitiveType> getPrimitiveType() { return PType; }
    void setArrayType(std::shared_ptr<ArrayType> A) { AType = A; }
    std::shared_ptr<ArrayType> getArrayType() { return AType; }
    void toPrint(std::ofstream &File);
  };

  class Symbol {
  public:
    virtual ~Symbol() = default;
    virtual std::shared_ptr<Type> getType() = 0;
    virtual SymbolType getSymbolType() = 0;
    virtual void toPrint(std::ofstream&) = 0;
  };
 
  class VariableSymbol : public Symbol {
    std::shared_ptr<Type> T;
  public:
    VariableSymbol(std::shared_ptr<Type> T) : T(T) {}
    std::shared_ptr<Type> getType() override;
    SymbolType getSymbolType() override;
    void toPrint(std::ofstream&) override;
  };

  class ProcedureSymbol : public Symbol {
    std::shared_ptr<Type> T;
  public:
    ProcedureSymbol(std::shared_ptr<Type> T) : T(T) {}
    std::shared_ptr<Type> getType() override;
    SymbolType getSymbolType() override;
    void toPrint(std::ofstream&) override;
  };

  class FunctionSymbol : public Symbol {
    std::shared_ptr<Type> T;
  public:
    FunctionSymbol(std::shared_ptr<Type> T) : T(T) {}
    std::shared_ptr<Type> getType() override;
    SymbolType getSymbolType() override;
    void toPrint(std::ofstream&) override;
  }; 

  class SymbolTable {
    std::map<std::string, std::shared_ptr<Symbol>> Table; 
  public:    
    SymbolTable() {}
    bool insert(const std::string&, std::shared_ptr<Symbol>);
    std::shared_ptr<Symbol> find(const std::string&);
    void toPrint(std::ofstream&);
  };
}
