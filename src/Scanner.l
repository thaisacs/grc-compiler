%option noyywrap nounput

%{
#include <iostream>
#include <memory>
#include <cstring>
#include "Parser.hpp"
%}

whitespace [ \r\f\v\t]
linefeed   \n
identifier _?[a-zA-Z]+_*[0-9]*[a-zA-Z]*
number     [0-9]+
comment    "//".*\n
string     "\"".*"\""

%%
"+" |
"-" |
"*" |
"/" |
"%" |
"{" |
"}" |
"(" |
")" |
"[" |
"]" |
"," |
";" { return yytext[0]; }

">"  { yylval.F = 1; return CMP; }
"<"  { yylval.F = 2; return CMP; }
">=" { yylval.F = 3; return CMP; }
"<=" { yylval.F = 4; return CMP; }

"==" { return EQUAL; }
"!=" { return DIFF;  }

"="  { return ASSIGN_INIT; }
"++" { yylval.F = 1; return ASSIGN_STEP; }
"--" { yylval.F = 2; return ASSIGN_STEP; }

"+=" { yylval.F = 1; return ASSIGN; }
"-=" { yylval.F = 2; return ASSIGN; }
"*=" { yylval.F = 3; return ASSIGN; }
"/=" { yylval.F = 4; return ASSIGN; }
"%=" { yylval.F = 5; return ASSIGN; }

"&&" { return AND;   }
"||" { return OR;    }
"!"  { return NOT;   }
"?"  { return QUEST; }
":"  { return POINTS;}

"int"    { return TYPE_INT;    }
"bool"   { return TYPE_BOOL;   }
"string" { return TYPE_STRING; }

"def"    { return DEF;    }
"else"   { return ELSE;   }
"false"  { return FALSE;  }
"for"    { return FOR;    }
"if"     { return IF;     }
"read"   { return READ;   }
"write"  { return WRITE;  }
"return" { return RETURN; }
"skip"   { return SKIP;   }
"stop"   { return STOP;   }
"true"   { return TRUE;   }
"var"    { return VAR;    }
"while"  { return WHILE;  }
"import" { return IMPORT; }
"io"     { return IO;     }

{identifier} { yylval.S = new char[std::strlen(yytext)];
               std::strcpy(yylval.S, yytext);
               return IDENTIFIER; }

{number} { yylval.N = atoi(yytext); return NUMBER; }

{string} { yylval.S = new char[std::strlen(yytext)]; 
           std::strcpy(yylval.S, yytext);
           return WORD; }

{comment}    { yylineno++; }
{linefeed}   { yylineno++; }
{whitespace} {             }

%%
