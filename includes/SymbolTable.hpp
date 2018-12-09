#pragma once

#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"

#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <fstream>

namespace grc {
  enum SymbolType {Variable, Procedure, Function};
  enum BasicType {Int, Bool, String, Void, Undefined, IntArray, BoolArray};

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
    Type(std::shared_ptr<PrimitiveType> PT, std::shared_ptr<ArrayType> AT) : 
      PType(PT), AType(AT) { }
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
    llvm::AllocaInst* Value;
    bool Argument; 
  public:
    VariableSymbol(std::shared_ptr<Type> T, bool Argument) : T(T), Argument(Argument) {}
    std::shared_ptr<Type> getType() override;
    SymbolType getSymbolType() override;
    void toPrint(std::ofstream&) override;
    bool getArgument() { return Argument; }
    llvm::AllocaInst* getValue() { return Value; }
    void setValue(llvm::AllocaInst *V) { Value = V; }
  };

  class ProcedureSymbol : public Symbol {
    std::shared_ptr<Type> T;
    std::vector<std::shared_ptr<Type>> TArgs;
  public:
    ProcedureSymbol(std::shared_ptr<Type> T, std::vector<std::shared_ptr<Type>> Args) : 
      T(T), TArgs(std::move(Args)) {}
    std::shared_ptr<Type> getType() override;
    SymbolType getSymbolType() override;
    unsigned getSizeArgs() { return TArgs.size(); }
    std::shared_ptr<Type> getTypeArg(int i) 
    { if(i < TArgs.size()) return TArgs[i]; else return nullptr; };
    void toPrint(std::ofstream&) override;
  };

  class FunctionSymbol : public Symbol {
    std::shared_ptr<Type> T;
    std::vector<std::shared_ptr<Type>> TArgs;
  public:
    FunctionSymbol(std::shared_ptr<Type> T, std::vector<std::shared_ptr<Type>> Args) : 
      T(T), TArgs(std::move(Args)) {}
    std::shared_ptr<Type> getType() override;
    SymbolType getSymbolType() override;
    std::shared_ptr<Type> getTypeArg(int i) 
    { if(i < TArgs.size()) return TArgs[i]; else return nullptr; };
    unsigned getSizeArgs() { return TArgs.size(); }
    void toPrint(std::ofstream&) override;
  }; 

  class SymbolTable {
    std::map<std::string, std::shared_ptr<Symbol>> Table; 
  public:    
    SymbolTable() {}
    bool insert(const std::string&, std::shared_ptr<Symbol>);
    std::shared_ptr<Symbol> find(const std::string&);
    std::shared_ptr<Symbol> findCurrentSub();
    void toPrint(std::ofstream&);
  };
}
