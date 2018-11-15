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
  #include "Error.hpp"
  #include "Util.hpp"
  #include "Scope.hpp"
  #include "Log.hpp"

  extern int yylex();
  extern llvm::LLVMContext TheContext;
  extern std::unique_ptr<llvm::Module> TheModule;
  extern std::shared_ptr<grc::Scope> S;
  extern std::unique_ptr<grc::Log> LOG;
  
  extern int yylineno;
  extern char* yytext;
  void yyerror(const char *s);
%}

%start program

%union {
  char *S;
  int N;  
  uint8_t F;
  grc::ExprAST *E;
  grc::VarExprAST* VE;
  grc::PrimitiveType *PMT;
  grc::BlockExprAST* B;
  grc::PrototypeAST* PT;
  VariableAndType *V;
  Variables *VS; 
  Booleans *LB;
  Integers *LI;
  Parameter *P;
  Parameters *PS;
  Expressions *Expr;
  //grc::Type* T;
  //grc::CallExprAST* CE;
  //grc::WriteExprAST* WE;
}

%nonassoc END_ELSE 
%nonassoc ELSE

%token DEF FOR IF READ RETURN SKIP STOP VAR WHILE WRITE TRUE FALSE IMPORT IO
%token ASSIGN_INIT TYPE_INT TYPE_BOOL TYPE_STRING

%token <S> IDENTIFIER 
%token <S> WORD 
%token <N> NUMBER 
%token <F> ASSIGN_STEP ASSIGN

%left OR
%left AND 
%left EQUAL DIFF
%nonassoc <F> CMP
%left '+' '-'
%left '*' '%' '/'
%nonassoc NOT UMINUS

%type <E> exp boolExp  intExp arrayInitVar cmd writeCmd  simpleCmd
/*
ifCmd whileCmd body assignInit assign
*/
%type <V> var simpleVar simpleInitVar arrayVar 
%type <PMT> type
%type <VS> listOfVarH listOfVarB
%type <LB> listOfBoolH listOfBoolB 
%type <LI> listOfIntH listOfIntB
%type <VE> varCmd 
%type <P> parameter
%type <PS> parametersH parametersB listOfParamB listOfParamH
%type <B> block
%type <Expr> cmds writeCmdH writeCmdB
%type <PT> prototype
/*
%type <PS> parameters subParameter listOfParameters
%type <VE> listOfVarH listOfVarB varCmd
%type <Expr> listOfCallValues bodyWrite
%type <CE> callSubCmd
%type <WE> writeCmd
*/
%%

program: import vars decs { VerifyMain(); }
       ;

import: /*empty*/
      | IMPORT IO { HandleImportIO(); }

decs: /*empty*/
    | decs decSub { /*default*/ }
    ;

vars: /*empty*/
    | vars varCmd { LOG->variable($2); }

type: TYPE_INT                   { $$ = HandleType(grc::BasicType::Int, 0);     }
    | TYPE_BOOL                  { $$ = HandleType(grc::BasicType::Bool, 0);    }
    | TYPE_STRING                { $$ = HandleType(grc::BasicType::String, 0);  }
    | TYPE_STRING '[' NUMBER ']' { $$ = HandleType(grc::BasicType::String, $3); }
    ;

prototype: DEF IDENTIFIER '(' listOfParamH ')'          { $$ = HandlePrototype($2, $4);     }
         | DEF IDENTIFIER '(' listOfParamH ')' ':' type { $$ = HandlePrototype($2, $4, $7); }
         ; 

decSub: prototype block { auto Sub = HandleSubroutine($1, $2); 
                          if(Sub) {
                            std::unique_ptr<grc::SubroutineAST> SubAST(Sub);
                            auto SubIR = SubAST->codegen();
                            LOG->subroutine(Sub);
                            LOG->scopes(S); 
                            S->finalizeScope();
                          }
                        }
      ;

listOfParamH: /*empty*/                         { $$ = HandleListOfParams();               }
            | listOfParamB parametersH ':' type { HandleListOfParams($1, $2, $4); $$ = $1; }
            ;

listOfParamB: /*empty*/      { $$ = HandleListOfParams(); } 
          | listOfParamH ';' { $$ = $1;                   }
          ;

parametersH: parametersB parameter { HandleParameters($1, $2); $$ = $1; }
           ;

parametersB: /*empty*/       { $$ = HandleParameters(); }
           | parametersH ',' { /*default*/              }
           ;

parameter: IDENTIFIER         { $$ = HandleParameter($1, false); }
         | IDENTIFIER '[' ']' { $$ = HandleParameter($1, true);  }
         ;

block: '{' cmds '}' { $$ = HandleBlock($2); }
     ;

cmds: /*empty*/  { $$ = HandleCmd();           }
    | cmds cmd   { HandleCmd($1, $2); $$ = $1; }
/*  | cmds block { /*HandleCmd($1, $2); $$ = $1; }*/
    ;

cmd: simpleCmd { /*default*/ }
   ;
/*
body: block { /*default* }
    | cmd   { /*default* }
    ;
*/
simpleCmd: varCmd     { /*default*/ }
         | writeCmd   { /*default*/ }
/*       | ifCmd      { default }
         | whileCmd   { default }
         | assignCmd  { default }
         | forCmd     { default }
         | varCmd     { default }
         | readCmd    { default }
         | callSubCmd { default }*/
         ;
/*
ifCmd: IF '(' exp ')' body %prec END_ELSE { /*$$ = HandleCmdIf($3, $5);*     }
     | IF '(' exp ')' body ELSE body      { /*$$ = HandleCmdIf($3, $5, $7);* }
     ;

whileCmd: WHILE '(' exp ')' body { /*$$ = HandleCmdWhile($3, $5);* }
        ;

forCmd: FOR '(' assignInit ';' exp ';' assign ')' {}
       ;
*/

/*
assignCmd: assign ';'     { /*default* }
         | assignInit ';' { /*default** }
         ;
        
assign: IDENTIFIER ASSIGN exp  { /*$$ = HandleAssign($1, $2, $3);* }
      | IDENTIFIER ASSIGN_STEP { /*$$ = HandleAssign($2, $1);*     }
      | ASSIGN_STEP IDENTIFIER { /*$$ = HandleAssign($1, $2);*     }
      ;

assignInit: IDENTIFIER ASSIGN_INIT exp { /*$$ = HandleAssign("=", $1, $3); }
          ;

readCmd: READ IDENTIFIER ';' {}
       ;
*/
writeCmd: WRITE writeCmdH ';' { $$ = HandleCmdWrite($2); }
        ;

writeCmdH: writeCmdB exp  { HandleCmdWrite($1, $2); $$ = $1; }
         | writeCmdB WORD { HandleCmdWrite($1, $2); $$ = $1; }
         ;

writeCmdB: /*empty*/     { $$ = HandleCmdWrite(); }
         | writeCmdH ',' { /*default*/            }
         ;
/*
bodyWrite: /*empty*         { /*$$ = HandleCmdWrite();*           }
         | bodyWrite exp     { /*HandleCmdWrite($1, $2); $$ = $1;* }
         | bodyWrite ',' exp { /*HandleCmdWrite($1, $3); $$ = $1;* }
         ;
/*
callSubCmd: IDENTIFIER '(' listOfCallValues ')' ';' { /*$$ = HandleCmdCall($1, $3); } 
          ;

listOfCallValues: /*empty                { /*$$ = HandleCmdCall();           }
                | listOfCallValues exp     { /*HandleCmdCall($1, $2); $$ = $1; } 
                | listOfCallValues ',' exp { /*HandleCmdCall($1, $3); $$ = $1; }
*/
varCmd: VAR listOfVarH ':' type ';' { $$ = HandleVarCmd($2, $4);                                 }
      | error ';'                   { HandleError("invalid variable declaration"); $$ = nullptr; }
      ;

listOfVarH: listOfVarB var { HandleListOfVar($1, $2); $$ = $1; }
         ;

listOfVarB: /*empty*/      { $$ = HandleListOfVar(); } 
          | listOfVarH ',' { $$ = $1;                }
          ;

var: simpleVar     { /*default*/ }
   | simpleInitVar { /*default*/ } 
   | arrayVar      { /*default*/ }
   ;

simpleVar: IDENTIFIER { $$ = HandleVar($1); }
         ;

simpleInitVar: IDENTIFIER ASSIGN_INIT exp  { $$ = HandleVar($1, $3); }
             | IDENTIFIER ASSIGN_INIT WORD { $$ = HandleVar($1, $3); }
             ;

arrayVar: IDENTIFIER '[' NUMBER ']' arrayInitVar { $$ = HandleVar($1, $3, $5); }
        ;

arrayInitVar: /*empty*/           { $$ = nullptr; }
            | ASSIGN_INIT boolExp { $$ = $2;      }
            | ASSIGN_INIT intExp  { $$ = $2;      }
            ;

boolExp: '{' listOfBoolH '}' { $$ = HandleBool($2); }
       ;

intExp: '{' listOfIntH '}' { $$ = HandleInt($2); }
      ;

listOfBoolH: listOfBoolB FALSE { HandleBool($1, false); $$ = $1; }
           | listOfBoolB TRUE  { HandleBool($1, true); $$ = $1;  }
           ;

listOfBoolB: /*empty*/       { $$ = HandleListOfBool(); }
           | listOfBoolH ',' { $$ = $1;                 }
           ;

listOfIntH: listOfIntB NUMBER { HandleInt($1, $2); $$ = $1; }
          ;

listOfIntB: /*empty*/      { $$ = HandleListOfInt();  }
          | listOfIntH ',' { $$ = $1;                 }
          ;

exp: '(' exp ')'          { $$ = $2;                             }
   | exp '+' exp          { $$ = HandleExpression("+", $1, $3);  }
   | exp '*' exp          { $$ = HandleExpression("*", $1, $3);  }
   | exp '-' exp          { $$ = HandleExpression("-", $1, $3);  }
   | exp '/' exp          { $$ = HandleExpression("/", $1, $3);  }
   | exp '%' exp          { $$ = HandleExpression("%", $1, $3);  }
   | exp CMP exp          { $$ = HandleExpression($2, $1, $3);   }
   | exp EQUAL exp        { $$ = HandleExpression("==", $1, $3); }
   | exp DIFF exp         { $$ = HandleExpression("!=", $1, $3); }
   | exp AND exp          { $$ = HandleExpression("&&", $1, $3); }
   | exp OR exp           { $$ = HandleExpression("||", $1, $3); }
   | '-' exp %prec UMINUS { $$ = HandleExpression("-", $2);      }
   | NOT exp              { $$ = HandleExpression("!", $2);      }
   | NUMBER               { $$ = new grc::NumberExprAST($1);     }
   | IDENTIFIER           { $$ = new grc::VariableExprAST($1);   }
   | TRUE                 { $$ = new grc::BooleanExprAST(true);  }
   | FALSE                { $$ = new grc::BooleanExprAST(false); }
   ;
%%

void yyerror(const char *s) {
  fprintf(stderr, "error [%d]: %s: ",yylineno, s);
}
