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
  extern bool isError, doLog;

  extern int yylineno;
  extern char* yytext;
  void yyerror(const char *s);
%}

%define parse.error verbose

%start program

%union {
  char *S;
  int N;  
  uint8_t F;
  grc::ExprAST *E;
  grc::VarExprAST *VE;
  grc::PrimitiveType *PMT;
  grc::BlockExprAST *B;
  grc::PrototypeAST *PT;
  grc::VariableExprAST *VAR;
  VariableAndType *V;
  Variables *VS; 
  Booleans *LB;
  Integers *LI;
  Parameter *P;
  Parameters *PS;
  Expressions *Expr;
}

%nonassoc END_ELSE 
%nonassoc ELSE

%token DEF FOR IF READ RETURN SKIP STOP VAR WHILE WRITE TRUE FALSE IMPORT IO
%token ASSIGN_INIT TYPE_INT TYPE_BOOL TYPE_STRING

%token <S> IDENTIFIER 
%token <S> WORD 
%token <N> NUMBER 
%token <F> ASSIGN_STEP ASSIGN

%right QUEST
%left OR
%left AND 
%left EQUAL DIFF
%nonassoc <F> CMP
%left '+' '-'
%left '*' '%' '/'
%nonassoc NOT UMINUS

%type <Expr> cmds writeCmdH writeCmdB listOfCallValH listOfCallValB
%type <PS> parametersH parametersB listOfParamB listOfParamH listOfParamT listOfParams
%type <P> parameter
%type <PT> prototype
%type <VE> varCmd 
%type <V> var
%type <VS> listOfVarH listOfVarB
%type <LB> listOfBoolH listOfBoolB 
%type <LI> listOfIntH listOfIntB
%type <E> exp boolExp intExp arrayInitVar expAssign cmd simpleCmd writeCmd 
          callSub returnCmd readCmd assign assignInit ifCmd  whileCmd forCmd 
          /*stopCmd skipCmd */
%type <V> simpleVar simpleInitVar arrayVar
%type <PMT> type
%type <B> block bodyH
%type <VAR> variable

%%

program: import vars decs { VerifyMain(); }
       ;

import: /*empty*/
      | IMPORT IO { HandleImportIO(); }

decs: /*empty*/
    | decs decSub { /*default*/ }
    ;

vars: /*empty*/
    | vars varCmd { if($2) $2->globalCodegen();  }

type: TYPE_INT                   { $$ = HandleType(grc::BasicType::Int, 0);     }
    | TYPE_BOOL                  { $$ = HandleType(grc::BasicType::Bool, 0);    }
    | TYPE_STRING                { $$ = HandleType(grc::BasicType::String, 255); }
    | TYPE_STRING '[' NUMBER ']' { $$ = HandleType(grc::BasicType::String, $3); }
    ;

prototype: DEF IDENTIFIER '(' listOfParams ')'          { $$ = HandlePrototype($2, $4);     }
         | DEF IDENTIFIER '(' listOfParams ')' ':' type { $$ = HandlePrototype($2, $4, $7); }
         ; 

decSub: prototype block { if($1 && $2) {
                            auto Sub = HandleSubroutine($1, $2); 
                            if(Sub && !isError) {
                              std::unique_ptr<grc::SubroutineAST> SubAST(Sub);
                              auto SubIR = SubAST->codegen();
                              if(doLog) LOG->scopes(S); 
                            }else {
                              exit(0);
                            }
                          }
                          S->finalizeScope();
                        }
      ;

listOfParams: /*empty*/    { $$ = HandleListOfParams(); }
            | listOfParamH { /*default*/ }

listOfParamH: listOfParamB listOfParamT { HandleListOfParams($1, $2); $$ = $1; }
            ;

listOfParamT: parametersH ':' type { HandleListOfParams($1, $3); $$ = $1; }
            ;

listOfParamB: /*empty*/      { $$ = HandleListOfParams(); } 
          | listOfParamH ';' { $$ = $1;                   }
          ;

parametersH: parameter parametersB { HandleParameters($2, $1); $$ = $2; }
           ;

parametersB: /*empty*/       { $$ = HandleParameters(); }
           | ',' parametersH { $$ = $2;              }
           ;

parameter: IDENTIFIER         { $$ = HandleParameter($1, false); }
         | IDENTIFIER '[' ']' { $$ = HandleParameter($1, true);  }
         ;

block: '{' cmds '}' { $$ = HandleBlock($2); }
     ;

cmds: /*empty*/  { $$ = HandleCmd();           }
    | cmds cmd   { HandleCmd($1, $2); $$ = $1; }
    ;

cmd: simpleCmd { /*default*/ }
   ;

simpleCmd: varCmd     { /*default*/    }
         | writeCmd   { /*default*/    }
         | returnCmd  { /*default*/    }
         | callSubCmd { /*default*/    }
         | readCmd    { /*default*/    }
         | assignCmd  { /*default*/    }
         | ifCmd      { /*default*/    }
         | whileCmd   { /*default*/    }
         | forCmd     { /*default*/    }
/*       | stopCmd    { /*default*    }
         | skipCmd    { /*default*    }*/
         | error ';'  { LogError(""); 
                        $$ = nullptr;  }
         ;

bodyH: bodyB block bodyC { $$ = $2;    }
     | cmd               { /*default*/ }
     ;

bodyB: /*empty*/  { if(doLog) LOG->scopes(S); S->initializeScope(); }
     ;

bodyC: /*empty*/ { if(doLog) LOG->scopes(S); /*S->finalizeScope();*/ }
     ;

ifCmd: IF '(' exp ')' bodyH %prec END_ELSE { $$ = HandleCmdIf($3, $5);     }
     | IF '(' exp ')' bodyH ELSE bodyH     { $$ = HandleCmdIf($3, $5, $7); }
     ;

variable: IDENTIFIER             { $$ = HandleVariable($1);     }
        | IDENTIFIER '[' exp ']' { $$ = HandleVariable($1, $3); }

assignCmd: assign ';'     { /*default*/ }
         | assignInit ';' { /*default*/ }
         ;
        
assign: variable ASSIGN expAssign  { $$ = HandleAssign($1, $2, $3); }
      | variable ASSIGN_STEP       { $$ = HandleAssign($2, $1);     }
      | ASSIGN_STEP variable       { $$ = HandleAssign($1, $2);     }
      ;

assignInit: variable ASSIGN_INIT expAssign { $$ = HandleAssign("=", $1, $3); }
          ;

expAssign: exp        { /*default*/ }
         | callSub    { /*default*/ }
/*       | ternaryCmd { /*default* }*/
         ;

readCmd: READ IDENTIFIER ';'             { $$ = HandleCmdRead($2);     }
       | READ IDENTIFIER '[' exp ']' ';' { $$ = HandleCmdRead($2, $4); }
       ;

writeCmd: WRITE writeCmdH ';' { $$ = HandleCmdWrite($2); }
        ;

writeCmdH: writeCmdB exp      { HandleCmdWrite($1, $2); $$ = $1; }
 /*      | writeCmdB callSub  { HandleCmdWrite($1, $2); $$ = $1; }*/
         ;

writeCmdB: /*empty*/     { $$ = HandleCmdWrite(); }
         | writeCmdH ',' { /*default*/            }
         ;

returnCmd: RETURN exp ';' { $$ = HandleCmdReturn($2);      }
         | RETURN ';'     { $$ = HandleCmdReturn(nullptr); }
         ;

whileCmd: WHILE '(' exp ')' bodyH { $$ = HandleCmdWhile($3, $5); }
        ;

forCmd: FOR '(' assignInit ';' exp ';' assign ')' bodyH { $$ = HandleCmdFor($3, $5, $7, $9); }
      ;

callSubCmd: callSub ';' { /*default*/ } 
          ;

callSub: IDENTIFIER '(' listOfCallValH ')' { $$ = HandleCmdCall($1, $3); }

listOfCallValH: /*empty*/              { $$ = HandleCmdCall();  }
              | listOfCallValB exp     { HandleCmdCall($1, $2); } 
              | listOfCallValB callSub { HandleCmdCall($1, $2); } 

listOfCallValB: /*empty*/          { $$ = HandleCmdCall(); }
              | listOfCallValH ',' { /*default*/           }
              ;
/*
stopCmd: STOP ';' { $$ = HandleCmdStopOrSkip(grc::BasicItCmd::Stop); }
       ;

skipCmd: SKIP ';' { $$ = HandleCmdStopOrSkip(grc::BasicItCmd::Skip); }
       ;
*/
varCmd: VAR listOfVarH ':' type ';' { $$ = HandleVarCmd($2, $4); }
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

simpleVar: IDENTIFIER                 { $$ = HandleVar($1);     }
         | IDENTIFIER '[' NUMBER ']'  { $$ = HandleVar($1, $3); }

simpleInitVar: IDENTIFIER ASSIGN_INIT expAssign  { $$ = HandleVar("=", $1, $3); }
             ;

arrayVar: IDENTIFIER '[' NUMBER ']' arrayInitVar { $$ = HandleVar($1, $3, $5); }
        ;

arrayInitVar: ASSIGN_INIT boolExp { $$ = $2; }
            | ASSIGN_INIT intExp  { $$ = $2; }
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

listOfIntB: /*empty*/      { $$ = HandleListOfInt(); }
          | listOfIntH ',' { $$ = $1;                }
          ;
/*
ternaryCmd: exp '?' exp ':' exp {}
          ;
*/
exp: '(' exp ')'            { $$ = $2;                             }
   | exp '+' exp            { $$ = HandleExpression("+", $1, $3);  }
   | exp '*' exp            { $$ = HandleExpression("*", $1, $3);  }
   | exp '-' exp            { $$ = HandleExpression("-", $1, $3);  }
   | exp '/' exp            { $$ = HandleExpression("/", $1, $3);  }
   | exp '%' exp            { $$ = HandleExpression("%", $1, $3);  }
   | exp CMP exp            { $$ = HandleExpression($2, $1, $3);   }
   | exp EQUAL exp          { $$ = HandleExpression("==", $1, $3); }
   | exp DIFF exp           { $$ = HandleExpression("!=", $1, $3); }
   | exp AND exp            { $$ = HandleExpression("&&", $1, $3); }
   | exp OR exp             { $$ = HandleExpression("||", $1, $3); }
   | '-' exp %prec UMINUS   { $$ = HandleExpression("-", $2);      }
   | NOT exp                { $$ = HandleExpression("!", $2);      }
   | IDENTIFIER             { $$ = HandleVariable($1);             }
   | IDENTIFIER '[' exp ']' { $$ = HandleVariable($1, $3);         }
   | NUMBER                 { $$ = new grc::NumberExprAST($1);     }
   | TRUE                   { $$ = new grc::BooleanExprAST(true);  }
   | FALSE                  { $$ = new grc::BooleanExprAST(false); }
   | WORD                   { $$ = new grc::StringExprAST($1);     }
   ;
%%

void yyerror(const char *s) {
  fprintf(stderr, "error [%d]: %s\n",yylineno, s);
  isError = true;
}
