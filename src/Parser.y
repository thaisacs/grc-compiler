%code requires {
  #include "AST.hpp"
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

  extern int yylex();
  extern llvm::LLVMContext TheContext;
  extern std::unique_ptr<llvm::Module> TheModule;

  void yyerror(const char *s);
%}

%start program

%union {
  char* str;
  int num;  
  uint8_t fn;
  grc::ExprAST* E;
  grc::BlockAST* B;
}

%token DEF FOR IF READ RETURN SKIP STOP VAR WHILE WRITE TRUE FALSE EOL 
       ATRIBI T_INT T_BOOL T_STRING STRING
%token <str> IDENTIFIER 
%token <num> NUMBER 
%token <fn>  ATRIBS ATRIB

%nonassoc END_ELSE 
%nonassoc ELSE

%left OR
%left AND 
%left EQUAL DIFF
%left <fn> CMP
%left '+' '-'
%left '*' '%' '/'
%right NOT

%type <E> exp cmd cmdIf body
%type <B> cmds block
%%

program: block { $1->toPrint(); };
/*
program: decs 
       ;

decs: /*empty
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
              std::cout << $2 << std::endl; 
              $5->toPrint();
              std::cout << "\n";
              //auto proto = llvm::make_unique<grc::PrototypeAST>("fd"); 
              //auto b = llvm::make_unique<>();
              //auto PrcAST = llvm::make_unique<grc::ProcedureAST>(std::move(p));
              //PrcAST->codegen(TheContext);
              //F->print(llvm::errs());
              //fprintf(stderr, "\n");
            }
       ;

decFunc: DEF IDENTIFIER '(' ')' ':' type block { std::cout << $2 << std::endl;
                                               }
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
   /*    | cmdFor      { /*default }
         | cmdAtrib    { /*default }
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
