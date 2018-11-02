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
  #include "AST.hpp"

  extern int yylex();
  extern llvm::LLVMContext TheContext;
  extern std::unique_ptr<llvm::Module> TheModule;

  void yyerror(const char *s);
%}

%start program

%union {
  char* str;
  int num;  
  int fn;
  grc::ExprAST* E;
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

%type <E> exp stmts stmtIf commands block
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
block: '{' decs commands '}' { $$ = $3; }
     ;

commands: /*empty*/      { $$ = nullptr; }
        | commands stmts { $$ = $2; };

stmts: simpleStmt { /*default*/ }
     | block      { /*default*/ };

simpleStmt: stmtIf       { /*default*/ }
          | stmtWhile    { /*default*/ }
          | stmtFor      { /*default*/ }
          | stmtAtrib    { /*default*/ }
          | stmtCallProc { /*default*/ }
          ;

stmtIf: IF '(' exp ')' stmts %prec END_ELSE { std::unique_ptr<grc::ExprAST> exp($3);
                                              $$ = new grc::IfExprAST(std::move(exp), nullptr, nullptr); }
      | IF '(' exp ')' stmts ELSE stmts { std::unique_ptr<grc::ExprAST> exp($3);
                                          $$ = new grc::IfExprAST(std::move(exp), nullptr, nullptr); }
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
exp: '(' exp ')' { $$ = $2; }
   | exp '+' exp { std::unique_ptr<grc::ExprAST> e($1);
                   std::unique_ptr<grc::ExprAST> d($3);
                   $$ = new grc::BinaryExprAST("+", std::move(e), std::move(d)); }
   | exp '*' exp { std::unique_ptr<grc::ExprAST> e($1);
                   std::unique_ptr<grc::ExprAST> d($3);
                   $$ = new grc::BinaryExprAST("*", std::move(e), std::move(d)); }
   | exp '-' exp { std::unique_ptr<grc::ExprAST> e($1);
                   std::unique_ptr<grc::ExprAST> d($3);
                   $$ = new grc::BinaryExprAST("-", std::move(e), std::move(d)); }
   | exp '/' exp { std::unique_ptr<grc::ExprAST> e($1);
                   std::unique_ptr<grc::ExprAST> d($3);
                   $$ = new grc::BinaryExprAST("/", std::move(e), std::move(d)); }
   | exp '%' exp { std::unique_ptr<grc::ExprAST> e($1);
                   std::unique_ptr<grc::ExprAST> d($3);
                   $$ = new grc::BinaryExprAST("%", std::move(e), std::move(d)); }
   | exp CMP exp { std::unique_ptr<grc::ExprAST> e($1);
                   std::unique_ptr<grc::ExprAST> d($3);
                   switch($2) {
                    case 1:
                      $$ = new grc::BinaryExprAST(">", std::move(e), std::move(d));
                      break;
                    case 2:
                      $$ = new grc::BinaryExprAST("<", std::move(e), std::move(d));
                      break;
                    case 3:
                      $$ = new grc::BinaryExprAST("<>", std::move(e), std::move(d));
                      break;
                    case 4:
                      $$ = new grc::BinaryExprAST(">=", std::move(e), std::move(d));
                      break;
                    case 5:
                      $$ = new grc::BinaryExprAST("<=", std::move(e), std::move(d));
                   } 
                 }
   | exp EQUAL exp { std::unique_ptr<grc::ExprAST> e($1);
                     std::unique_ptr<grc::ExprAST> d($3);
                     $$ = new grc::BinaryExprAST("==", std::move(e), std::move(d)); }
   | exp DIFF exp  { std::unique_ptr<grc::ExprAST> e($1);
                     std::unique_ptr<grc::ExprAST> d($3);
                     $$ = new grc::BinaryExprAST("!=", std::move(e), std::move(d)); }
   | exp AND exp   { std::unique_ptr<grc::ExprAST> e($1);
                     std::unique_ptr<grc::ExprAST> d($3);
                     $$ = new grc::BinaryExprAST("&&", std::move(e), std::move(d)); }
   | exp OR exp    { std::unique_ptr<grc::ExprAST> e($1);
                     std::unique_ptr<grc::ExprAST> d($3);
                     $$ = new grc::BinaryExprAST("||", std::move(e), std::move(d)); }
   | NOT exp       { std::unique_ptr<grc::ExprAST> d($2);
                     $$ = new grc::UnaryExprAST('!', std::move(d)); }
   | NUMBER        { $$ = new grc::NumberExprAST($1);      }
   | IDENTIFIER    { $$ = new grc::VariableExprAST($1);    }
   | TRUE          { $$ = new grc::BooleanExprAST(true);   }
   | FALSE         { $$ = new grc::BooleanExprAST(false);  }
/* | callProc    {}*/
   ;
%%

void yyerror(const char *s) {
  printf("\nerror: %s\n", s);
  exit(1);
}
