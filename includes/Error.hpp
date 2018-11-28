#pragma once

#include <iostream>
#include <string>

#include "SymbolTable.hpp" 
#include "AST.hpp" 

void LogError(const std::string&);
void LogError(const std::string&, unsigned);
void LogWarning(const std::string&, unsigned); 
