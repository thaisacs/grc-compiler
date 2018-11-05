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

  extern int yylex();
  extern llvm::LLVMContext TheContext;
  extern std::unique_ptr<llvm::Module> TheModule;
  extern std::shared_ptr<grc::Scope> S;

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
  Parameters* PS;
  Parameter* P;
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

%type <E> exp cmd cmdIf body
%type <B> cmds block
%type <P> parameter
%type <PS> parameters subParameter listOfParameters
%type <T> type
%%

program: decs 
       ;

decs: /*empty*/
/*  | decs decVar { } */
    | decs decSub { }
    ;

/*
decVar: VAR listOfVarIdent ':' type ';' {}
      ;
*/

prototype: DEF IDENTIFIER '(' listOfParameters ')' { HandlePrototype($2, $4); }
/*       | DEF IDENTIFIER '(' ')' ':' type { HandlePrototype($2, grc::INT);  } */
         ; 

type: TYPE_INT                   { $$ = HandleType(grc::INT, 0);     }
    | TYPE_BOOL                  { $$ = HandleType(grc::BOOL, 0);    }
    | TYPE_STRING                { $$ = HandleType(grc::STRING, 0);  }
    | TYPE_STRING '[' NUMBER ']' { $$ = HandleType(grc::STRING, $3); }
    ;

decSub: prototype block { S->finalizeScope();
              //$5->toPrint();
              //auto proto = llvm::make_unique<grc::PrototypeAST>("fd"); 
              //auto b = llvm::make_unique<>();
              //auto PrcAST = llvm::make_unique<grc::ProcedureAST>(std::move(p));
              //PrcAST->codegen(TheContext);
              //F->print(llvm::errs());
              //fprintf(stderr, "\n");
            }
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

simpleCmd: cmdIf       { /*default*/ }
         | cmdWhile    { /*default*/ }
         | cmdAssign   { /*default*/ }
   /*    | cmdFor      { /*default }
         | cmdCallProc { /*default }*/
         ;

cmdIf: IF '(' exp ')' body %prec END_ELSE { $$ = HandleCmdIf($3, $5);     }
     | IF '(' exp ')' body ELSE body      { $$ = HandleCmdIf($3, $5, $7); }
     ;

cmdWhile: WHILE '(' exp ')' body {}
        ;
/*
stmtFor: FOR '(' atrib_init ';' exp ';' atrib ')' {}
       ;
*/
cmdAssign: assign ';'     {}
         | assignInit ';' {}
         ;
        
assign: IDENTIFIER ASSIGN exp {}
      | IDENTIFIER ASSIGN_STEP {}
      | ASSIGN_STEP IDENTIFIER {}
      ;

assignInit: IDENTIFIER ASSIGN_INIT exp { /*$$ = HandleExpression("=", nullptr, $3);*/ }
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
 *list of varible identifier
 
listOfVarIdent: var subListOfVarIdent {}
              | ',' var subListOfVarIdent {}
              ;

subListOfVarIdent: /*empty
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

ArrayInitVar: /*empty
            | ATRIBI listOfNumbers { }
            ;

listOfNumbers: '{' NUMBER subListOfNumbers '}' {}
             ;

subListOfNumbers: /*empty
                | ',' NUMBER subListOfNumbers {}
                ;
/*
 *expression
*/
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
/* | callProc    {}*/
   ;
%%

void yyerror(const char *s) {
  printf("\nerror: %s\n", s);
  exit(1);
}

