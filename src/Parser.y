%{
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/ADT/STLExtras.h"
#include <iostream>
#include "AST.hpp"

extern int yylex();
extern llvm::LLVMContext TheContext;
extern std::unique_ptr<llvm::Module> TheModule;
void yyerror(const char *s);
%}

%union {
  char *s;
  char *o;
  int n;  
  int fn;
}

%token <s> IDENTIFIER 
%token <o> STRING
%token <n> NUMBER 

%token DEF FOR IF READ RETURN SKIP STOP VAR WHILE WRITE TRUE FALSE 
%token EOL ATRIBI T_INT T_BOOL T_STRING

%token <fn> ATRIBS
%token <fn> ATRIB

%nonassoc END_ELSE 
%nonassoc ELSE

%left  <fn> CMP LOG
%left '*' '%' '/' '+' '-' '<' '>' 
%right NOT

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
              auto proto = llvm::make_unique<compiler::PrototypeAST>($2); 
              //auto b = llvm::make_unique<>();
              //auto PrcAST = llvm::make_unique<compiler::ProcedureAST>(std::move(p));
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

stmtIf: IF '(' exps ')' stmts %prec END_ELSE {}
      | IF '(' exps ')' stmts ELSE stmts {}
      ;

stmtWhile: WHILE '(' exps ')' stmts {}
         ;

stmtFor: FOR '(' atrib_init ';' exps ';' atrib ')' stmts {}
       ;

stmtAtrib: atrib ';' {}
         | atrib_init ';' {}
         ;
        
atrib: IDENTIFIER ATRIB exps {}
     | IDENTIFIER ATRIBS {}
     | ATRIBS IDENTIFIER {}
     ;

atrib_init: IDENTIFIER ATRIBI exps { }
          ;

stmtCallProc: callProc ';' {}
            ;
            
callProc: IDENTIFIER '(' lists ')' {} 
        ;

lists: /*empty */
     | list {};

list: exps {}
    | list ',' exps {};
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
exps: '(' exp ')'
    | exp;

exp: exps '+' exps {}
   | exps '*' exps {}
   | exps '-' exps {}
   | exps '/' exps {}
   | exps '%' exps {}
   | exps CMP exps {}
   | exps LOG exps {}
   | NOT exps      {}
   | callProc      {}
   | NUMBER        {}
   | IDENTIFIER    {}
   | TRUE          {}
   | FALSE         {}
   ;
%%

void yyerror(const char *s) {
  printf("\nerror: %s\n", s);
  exit(1);
}

//==========MIS=======
//expression: call sub
