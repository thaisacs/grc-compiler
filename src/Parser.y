%{
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/ADT/STLExtras.h"

#include "AST.hpp"

#include <iostream>
#include <memory>
#include <map>

extern int yylex();
extern llvm::LLVMContext TheContext;
extern std::unique_ptr<llvm::Module> TheModule;

void yyerror(const char *s);

%}

%union {
  char* str;
  int num;  
  int fn;
  void* E;
}

%token DEF FOR IF READ RETURN SKIP STOP
       VAR WHILE WRITE TRUE FALSE EOL 
       ATRIBI T_INT T_BOOL T_STRING

%token <str> IDENTIFIER 
%token <num> NUMBER 
%token <fn> ATRIBS
%token <fn> ATRIB
%token STRING

%nonassoc END_ELSE 
%nonassoc ELSE

%left '+' '-'
%left '*' '%' '/'
%left  <fn> CMP LOG
%right NOT

%type <E> exp
%start program

%%

program: decs 
       ;

decs: /*empty*/
    | decs decVar { }
    | decs decSub { }
    ;

decVar: VAR listOfVarIdent ':' type ';' {}
      ;

type: T_INT {}
    | T_BOOL {}
    | T_STRING {}
    | T_STRING '[' NUMBER ']' {}
    ;

decSub: decProc {}
      | decFunc {}
      ;

decProc: DEF IDENTIFIER '(' ')' block { 
              //auto proto = llvm::make_unique<grc::PrototypeAST>("fd"); 
              //auto b = llvm::make_unique<>();
              //auto PrcAST = llvm::make_unique<grc::ProcedureAST>(std::move(p));
              //PrcAST->codegen(TheContext);
              //F->print(llvm::errs());
              //fprintf(stderr, "\n");
            }
       ;

decFunc: DEF IDENTIFIER '(' ')' ':' type block {}
       ;
/*
listOfParameters: /*empty
                | parameters ':' TYPE listOfParameters {}
                | ';' parameters ':' TYPE listOfParameters {}
                ;

parameters: parameter subParameter
          | ',' parameter subParameter {}
          ;

subParameter: /*empty
            | parameters {}
            ;

parameter: IDENTIFIER {}
         | IDENTIFIER '[' ']' {}
         ;
*/
block: '{' decs commands '}' { std::cout << "block\n"; }
     ;

commands: /*empty*/
        | commands stmts;

stmts: simpleStmt {}
     | block;

simpleStmt: stmtIf {}
          | stmtWhile {}
          | stmtFor {}
          | stmtAtrib {}
          | stmtCallProc {}
          ;

stmtIf: IF '(' exp ')' stmts %prec END_ELSE {}
      | IF '(' exp ')' stmts ELSE stmts {}
      ;

stmtWhile: WHILE '(' exp ')' stmts {}
         ;

stmtFor: FOR '(' atrib_init ';' exp ';' atrib ')' stmts {}
       ;

stmtAtrib: atrib ';' {}
         | atrib_init ';' {}
         ;
        
atrib: IDENTIFIER ATRIB exp {}
     | IDENTIFIER ATRIBS {}
     | ATRIBS IDENTIFIER {}
     ;

atrib_init: IDENTIFIER ATRIBI exp { }
          ;

stmtCallProc: callProc ';' {}
            ;
            
callProc: IDENTIFIER '(' lists ')' {} 
        ;

lists: /*empty */
     | list {};

list: exp {}
    | list ',' exp {};
/*
 *list of varible identifier
 */
listOfVarIdent: var subListOfVarIdent {}
              | ',' var subListOfVarIdent {}
              ;

subListOfVarIdent: /*empty*/
                 | listOfVarIdent {}

var: SimpleVar {}
   | SimpleInitVar {}
   | ArrayVar {}
   ;

SimpleVar: IDENTIFIER {}
         ;

SimpleInitVar: atrib_init {}
             ;

ArrayVar: IDENTIFIER '[' NUMBER ']' ArrayInitVar { }
        ;

ArrayInitVar: /*empty*/
            | ATRIBI listOfNumbers { }
            ;

listOfNumbers: '{' NUMBER subListOfNumbers '}' {}
             ;

subListOfNumbers: /*empty*/
                | ',' NUMBER subListOfNumbers {}
                ;
/*
 *expression
*/
exp: '(' exp ')' {}
   | exp '+' exp { std::unique_ptr<grc::ExprAST> e(reinterpret_cast<grc::ExprAST*>($1));
                   std::unique_ptr<grc::ExprAST> d(reinterpret_cast<grc::ExprAST*>($3));
                   $$ = new grc::BinaryExprAST('+', std::move(e), std::move(d)); }
   | exp '*' exp {}
   | exp '-' exp {}
   | exp '/' exp {}
   | exp '%' exp {}
   | exp CMP exp {}
   | exp LOG exp {}
   | NOT exp     {}
   | NUMBER      { $$ = new grc::NumberExprAST($1); }
   | IDENTIFIER  { $$ = new grc::VariableExprAST($1); }
   | TRUE        { std::cout << "TRUE" << std::endl; }
   | FALSE       { std::cout << "FALSE" << std::endl; }
/* | callProc      {}*/
   ;
%%

void yyerror(const char *s) {
  printf("\nerror: %s\n", s);
  exit(1);
}

//==========MIS=======
//expression: call sub
