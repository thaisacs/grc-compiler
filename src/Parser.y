%code requires {
  #include "AST.hpp"
  #include "Util.hpp"
  #include "SymbolTable.hpp"
}

%{
  #include "llvm/IR/LLVMContext.h"
  #include "llvm/IR/Module.h"
  #include "llvm/ADT/STLExtras.h"
  
  #include <iostream>
  #include <memory>
  #include <map>
  #include <cstdint>
  
  #include "AST.hpp"
  #include "Util.hpp"
  #include "Scope.hpp"
  #include "Log.hpp"

  extern int yylex();
  extern llvm::LLVMContext TheContext;
  extern std::unique_ptr<llvm::Module> TheModule;
  extern std::shared_ptr<grc::Scope> S;
  extern std::unique_ptr<grc::Log> LOG;

  void yyerror(const char *s);
%}

%start program

%union {
  char* S;
  int N;  
  uint8_t F;
  grc::ExprAST* E;
  grc::BlockAST* B;
  grc::Type* T;
  grc::PrototypeAST* PT;
  Parameters* PS;
  Parameter* P;
  grc::VarExprAST* VE;
  grc::Variable* V;
}

%nonassoc END_ELSE 
%nonassoc ELSE

%token DEF FOR IF READ RETURN SKIP STOP VAR WHILE WRITE TRUE FALSE
%token WORD ASSIGN_INIT TYPE_INT TYPE_BOOL TYPE_STRING

%token <S> IDENTIFIER 
%token <N> NUMBER 
%token <F> ASSIGN_STEP ASSIGN

%left OR
%left AND 
%left EQUAL DIFF
%left <F> CMP
%left '+' '-'
%left '*' '%' '/'
%right NOT

%type <E> exp cmd ifCmd whileCmd body assignInit assign
%type <B> cmds block
%type <P> parameter
%type <PS> parameters subParameter listOfParameters
%type <T> type
%type <PT> prototype
%type <VE> listOfVarIdent varCmd
%type <V> var simpleVar simpleInitVar 

%%

program: decs
       ;

decs: /*empty*/
/*  | decs decVar { /*default } */
    | decs decSub { /*default*/ }
    ;
/*
decVar: VAR listOfVarIdent ':' type ';' { HandleListOfVar($2, $4); }
      ;
*/
prototype: DEF IDENTIFIER '(' listOfParameters ')' { $$ = HandlePrototype($2, $4); }
/*       | DEF IDENTIFIER '(' ')' ':' type { HandlePrototype($2, grc::INT);  } */
         ; 

type: TYPE_INT                   { $$ = HandleType(grc::INT, 0);     }
    | TYPE_BOOL                  { $$ = HandleType(grc::BOOL, 0);    }
    | TYPE_STRING                { $$ = HandleType(grc::STRING, 0);  }
    | TYPE_STRING '[' NUMBER ']' { $$ = HandleType(grc::STRING, $3); }
    ;

decSub: prototype block { auto Proc = HandleProcedure($1, $2); 
                          LOG->procedure(Proc);
                          //std::unique_ptr<grc::ProcedureAST> ProcAST(Proc);
                          //auto ProcIR = ProcAST->codegen();
                          S->finalizeScope();
                          //ProcIR->print(llvm::errs());
                          //fprintf(stderr, "\n");
                          /*F->print(llvm::errs());*/ }
      ;

listOfParameters: /*empty*/                                { $$ = HandleListOfParams();               }
                | listOfParameters parameters ':' type     { HandleListOfParams($1, $2, $4); $$ = $1; }
                | listOfParameters ';' parameters ':' type { HandleListOfParams($1, $3, $5); $$ = $1; }
                ;

parameters: subParameter parameter     { HandleParameters($1, $2); $$ = $1; }
          | subParameter ',' parameter { HandleParameters($1, $3); $$ = $1; }
          ;

subParameter: /*empty*/  { $$ = HandleParameters(); }
            | parameters { /*default*/              }
            ;

parameter: IDENTIFIER         { $$ = HandleParameter($1, false); }
         | IDENTIFIER '[' ']' { $$ = HandleParameter($1, true);  }
         ;

block: '{' cmds '}' { $$ = $2; }
     ;

cmds: /*empty*/  { $$ = HandleCmd();           }
    | cmds cmd   { HandleCmd($1, $2); $$ = $1; }
    | cmds block { HandleCmd($1, $2); $$ = $1; }
    ;

cmd: simpleCmd { /*default*/ }

body: block { /*default*/ }
    | cmd   { /*default*/ }
    ;

simpleCmd: ifCmd       { /*default*/ }
         | whileCmd    { /*default*/ }
         | assignCmd   { /*default*/ }
         | forCmd      { /*default*/ }
         | varCmd      { /*default*/ }
  /*     | cmdCallProc { /*default }*/
         ;

ifCmd: IF '(' exp ')' body %prec END_ELSE { $$ = HandleCmdIf($3, $5);     }
     | IF '(' exp ')' body ELSE body      { $$ = HandleCmdIf($3, $5, $7); }
     ;

whileCmd: WHILE '(' exp ')' body { $$ = HandleCmdWhile($3, $5); }
        ;

forCmd: FOR '(' assignInit ';' exp ';' assign ')' {}
       ;

varCmd: VAR listOfVarIdent ':' type ';' { HandleVarCmd($2, $4); $$ = $2; }
      ;

assignCmd: assign ';'     { /*default*/ }
         | assignInit ';' { /*default*/ }
         ;
        
assign: IDENTIFIER ASSIGN exp  { $$ = HandleAssign($1, $2, $3); }
      | IDENTIFIER ASSIGN_STEP { $$ = HandleAssign($2, $1);     }
      | ASSIGN_STEP IDENTIFIER { $$ = HandleAssign($1, $2);     }
      ;

assignInit: IDENTIFIER ASSIGN_INIT exp { $$ = HandleAssign("=", $1, $3); }
          ;
/*
stmtCallProc: callProc ';' {}
            ;
            
callProc: IDENTIFIER '(' lists ')' {} 
        ;

lists: /*empty 
     | list {};

list: exp {}
    | list ',' exp {};

/*
 *list of identifier variable
 */ 
listOfVarIdent: /*empty*/              { $$ = HandleListOfVar();           }
              | listOfVarIdent var     { HandleListOfVar($1, $2); $$ = $1; }
              | listOfVarIdent ',' var { HandleListOfVar($1, $3); $$ = $1; }
              ;

var: simpleVar     { /*default*/ }
   | simpleInitVar { /*default*/ } 
   | ArrayVar      { /*default*/ }
   ;

simpleVar: IDENTIFIER { $$ = HandleVar($1); }
         ;

simpleInitVar: IDENTIFIER ASSIGN_INIT exp { $$ = HandleVar($1, $3); }
             ;

ArrayVar: IDENTIFIER '[' NUMBER ']' ArrayInitVar { }
        ;

ArrayInitVar: /*empty*/                 { }
            | ASSIGN_INIT listOfNumbers { }
            ;

listOfNumbers: '{' NUMBER subListOfNumbers '}' {}
             ;

subListOfNumbers: /*empty*/                   {}
                | ',' NUMBER subListOfNumbers {}
                ;

exp: '(' exp ')'   { $$ = $2;                             }
   | exp '+' exp   { $$ = HandleExpression("+", $1, $3);  }
   | exp '*' exp   { $$ = HandleExpression("*", $1, $3);  }
   | exp '-' exp   { $$ = HandleExpression("-", $1, $3);  }
   | exp '/' exp   { $$ = HandleExpression("/", $1, $3);  }
   | exp '%' exp   { $$ = HandleExpression("%", $1, $3);  }
   | exp CMP exp   { $$ = HandleExpression($2, $1, $3);   }
   | exp EQUAL exp { $$ = HandleExpression("==", $1, $3); }
   | exp DIFF exp  { $$ = HandleExpression("!=", $1, $3); }
   | exp AND exp   { $$ = HandleExpression("&&", $1, $3); }
   | exp OR exp    { $$ = HandleExpression("||", $1, $3); }
   | NOT exp       { $$ = HandleExpression("!", $2);      }
   | NUMBER        { $$ = new grc::NumberExprAST($1);     }
   | IDENTIFIER    { $$ = new grc::VariableExprAST($1);   }
   | TRUE          { $$ = new grc::BooleanExprAST(true);  }
   | FALSE         { $$ = new grc::BooleanExprAST(false); }
/* | callProc    {}
   ;*/
%%

void yyerror(const char *s) {
  printf("\nerror: %s\n", s);
  exit(1);
}
