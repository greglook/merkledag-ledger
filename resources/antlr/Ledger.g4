// ledger grammar file
// vim: ft=antlr4

grammar ledger;

//@header        {package antlr_example;}
//@lexer::header {package antlr_example;}

//options {
//    output=AST;
//    ASTLabelType=CommonTree;
//}



///// LEXER RULES /////

PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';

fragment DIGIT: '0'..'9';
INT: (DIGIT)+;

//WHITESPACE: ( ' ' | '\t' | '\r' | '\n'| '\u000C' )+ -> channel(HIDDEN);


///// PARSER RULES /////

expr: term ((PLUS | MINUS) term)*;
term: factor ((MULT | DIV) factor)*;
factor: INT;
