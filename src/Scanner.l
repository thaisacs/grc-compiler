%option noyywrap nounput
%{
#include <iostream>
#include <memory>
#include <cstring>
#include "Parser.hpp"
%}

whitespace [ \r\f\v\t]
linefeed   \n
identifier _?[a-zA-Z]+[0-9]*[a-zA-Z]*
number     [0-9]+
comment    "//".*\n
string     "\"".*"\""

%%
"+" |
"-" |
"*" |
"/" |
"%" |
"?" |
":" |
"{" |
"}" |
"(" |
")" |
"[" |
"]" |
"," |
";" { return yytext[0]; }

"="  { return ATRIBI; }

">"  { yylval.fn = 1; return CMP; }
"<"  { yylval.fn = 2; return CMP; }
"<>" { yylval.fn = 3; return CMP; }
">=" { yylval.fn = 4; return CMP; }
"<=" { yylval.fn = 5; return CMP; }

"==" { return EQUAL; }
"!=" { return DIFF;  }

"++" { yylval.fn = 1; return ATRIBS; }
"--" { yylval.fn = 2; return ATRIBS; }
"+=" { yylval.fn = 1; return ATRIB; }
"-=" { yylval.fn = 2; return ATRIB; }
"*=" { yylval.fn = 3; return ATRIB; }
"/=" { yylval.fn = 4; return ATRIB; }
"%=" { yylval.fn = 5; return ATRIB; }

"&&" { return AND; }
"||" { return OR;  }
"!"  { return NOT; }

"int"    { return T_INT;    }
"bool"   { return T_BOOL;   }
"string" { return T_STRING; }

"def"    { return DEF;    }
"else"   { return ELSE;   }
"false"  { return FALSE;  }
"for"    { return FOR;    }
"if"     { return IF;     }
"read"   { return READ;   }
"return" { return RETURN; }
"skip"   { return SKIP;   }
"stop"   { return STOP;   }
"true"   { return TRUE;   }
"var"    { return VAR;    }
"while"  { return WHILE;  }
"write"  { return WRITE;  }

{identifier} { yylval.str = new char[std::strlen(yytext)];
               std::strcpy(yylval.str, yytext);
               return IDENTIFIER;
             }
{number}     { yylval.num = atoi(yytext); return NUMBER; }
{string}     { /*yylval.str = yytext;*/ return STRING;       }

{comment}    {             }
{whitespace} {             }
{linefeed}   { yylineno++; }

%%
