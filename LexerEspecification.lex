package scanner;

import java_cup.runtime.*;
import parser.Sym;

%%

%public
%class Lexer
%implements Sym
%unicode
%line
%column
%cup
//%cupdebug

%{
	StringBuffer string = new StringBuffer();
	private Symbol symbol(int type) {
		return new Symbol(type, yyline, yycolumn);
	}
	private Symbol symbol(int type, Object value) {
		return new Symbol(type, yyline, yycolumn, value);
	}
%}

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment} | {DocumentationComment}

TraditionalComment   = "/*" [^*] ~"*/" | "/*" "*"+ "/"
// Comment can be the last line of the file, without line terminator.
EndOfLineComment     = "//" {InputCharacter}* {LineTerminator}?
DocumentationComment = "/**" {CommentContent} "*"+ "/"
CommentContent       = ( [^*] | \*+ [^/*] )*
    
Identifier = [:jletter:] [:jletterdigit:]*

/* integer literals */
DecIntegerLiteral = 0 | [1-9][0-9]*
DecLongLiteral    = {DecIntegerLiteral} [lL]
/* floating point literals */        
//FloatLiteral  = ({FLit1}|{FLit2}|{FLit3}) {Exponent}? [fF]
DoubleLiteral = ({FLit1}|{FLit2}|{FLit3}) {Exponent}?

FLit1    = [0-9]+ \. [0-9]* 
FLit2    = \. [0-9]+ 
FLit3    = [0-9]+ 
Exponent = [eE] [+-]? [0-9]+

/* string and character literals */
//StringCharacter = [^\r\n\"\\]
//SingleCharacter = [^\r\n\'\\]

%state INIT
%%
<INIT>{
	\"                             { yybegin(YYINITIAL); return symbol(sym.STRING_LITERAL, string.toString()); }
    [^\n\r\"\\]+                   { string.append( yytext() ); }
    \\t                            { string.append('\t'); }
    \\n                            { string.append('\n'); }

    \\r                            { string.append('\r'); }
    \\\"                           { string.append('\"'); }
    \\                             { string.append('\\'); }
	
}

<YYINITIAL>{
 
 /* keywords */
  "abstract"                     { return symbol(ABSTRACT); }
  "boolean"                      { return symbol(BOOLEAN); }
  "break"                        { return symbol(BREAK); }
  "byte"                         { return symbol(BYTE); }
  "case"                         { return symbol(CASE); }
  "catch"                        { return symbol(CATCH); }
  "char"                         { return symbol(CHAR); }
  "class"                        { return symbol(CLASS); }
  "const"                        { return symbol(CONST); }
  "continue"                     { return symbol(CONTINUE); }
  "do"                           { return symbol(DO); }
  "double"                       { return symbol(DOUBLE); }
  "else"                         { return symbol(ELSE); }
  "extends"                      { return symbol(EXTENDS); }
  "final"                        { return symbol(FINAL); }
  "finally"                      { return symbol(FINALLY); }
  "float"                        { return symbol(FLOAT); }
  "for"                          { return symbol(FOR); }
  "default"                      { return symbol(DEFAULT); }
  "implements"                   { return symbol(IMPLEMENTS); }
  "import"                       { return symbol(IMPORT); }
  "instanceof"                   { return symbol(INSTANCEOF); }
  "int"                          { return symbol(INT); }
  "interface"                    { return symbol(INTERFACE); }
  "long"                         { return symbol(LONG); }
  "native"                       { return symbol(NATIVE); }
  "new"                          { return symbol(NEW); }
  "goto"                         { return symbol(GOTO); }
  "if"                           { return symbol(IF); }
  "public"                       { return symbol(PUBLIC); }
  "short"                        { return symbol(SHORT); }
  "super"                        { return symbol(SUPER); }
  "switch"                       { return symbol(SWITCH); }
  "synchronized"                 { return symbol(SYNCHRONIZED); }
  "package"                      { return symbol(PACKAGE); }
  "private"                      { return symbol(PRIVATE); }
  "protected"                    { return symbol(PROTECTED); }
  "transient"                    { return symbol(TRANSIENT); }
  "return"                       { return symbol(RETURN); }
  "void"                         { return symbol(VOID); }
  "static"                       { return symbol(STATIC); }
  "while"                        { return symbol(WHILE); }
  "this"                         { return symbol(THIS); }
  "throw"                        { return symbol(THROW); }
  "throws"                       { return symbol(THROWS); }
  "try"                          { return symbol(TRY); }
  "volatile"                     { return symbol(VOLATILE); }
  "strictfp"                     { return symbol(STRICTFP); }
	
	
	/* boolean literals */
  "true"                         { return symbol(BOOLEAN_LITERAL, true); }
  "false"                        { return symbol(BOOLEAN_LITERAL, false); }
  
  /* null literal */
  "null"                         { return symbol(NULL_LITERAL); }
  
  
  /* separators */
  "("                            { return symbol(LPAREN); }
  ")"                            { return symbol(RPAREN); }
  "{"                            { return symbol(LBRACE); }
  "}"                            { return symbol(RBRACE); }
  "["                            { return symbol(LBRACK); }
  "]"                            { return symbol(RBRACK); }
  ";"                            { return symbol(SEMICOLON); }
  ","                            { return symbol(COMMA); }
  "."                            { return symbol(DOT); }
  
  /* operators */
  "="                            { return symbol(EQ); }
  ">"                            { return symbol(GT); }
  "<"                            { return symbol(LT); }
  "!"                            { return symbol(NOT); }
  "~"                            { return symbol(COMP); }
  "?"                            { return symbol(QUESTION); }
  ":"                            { return symbol(COLON); }
  "=="                           { return symbol(EQEQ); }
  "<="                           { return symbol(LTEQ); }
  ">="                           { return symbol(GTEQ); }
  "!="                           { return symbol(NOTEQ); }
  "&&"                           { return symbol(ANDAND); }
  "||"                           { return symbol(OROR); }
  "++"                           { return symbol(PLUSPLUS); }
  "--"                           { return symbol(MINUSMINUS); }
  "+"                            { return symbol(PLUS); }
  "-"                            { return symbol(MINUS); }
  "*"                            { return symbol(MULT); }
  "/"                            { return symbol(DIV); }
  "&"                            { return symbol(AND); }
  "|"                            { return symbol(OR); }
  "^"                            { return symbol(XOR); }
  "%"                            { return symbol(MOD); }
  "<<"                           { return symbol(LSHIFT); }
  ">>"                           { return symbol(RSHIFT); }
  ">>>"                          { return symbol(URSHIFT); }
  "+="                           { return symbol(PLUSEQ); }
  "-="                           { return symbol(MINUSEQ); }
  "*="                           { return symbol(MULTEQ); }
  "/="                           { return symbol(DIVEQ); }
  "&="                           { return symbol(ANDEQ); }
  "|="                           { return symbol(OREQ); }
  "^="                           { return symbol(XOREQ); }
  "%="                           { return symbol(MODEQ); }
  "<<="                          { return symbol(LSHIFTEQ); }
  ">>="                          { return symbol(RSHIFTEQ); }
  ">>>="                         { return symbol(URSHIFTEQ); }
  
  /* string literal */
  \"                             { yybegin(STRING); string.setLength(0); }

  /* character literal */
  \'                             { yybegin(CHARLITERAL); }

  /* numeric literals */

  /* This is matched together with the minus, because the number is too big to 
     be represented by a positive integer. */
  "-2147483648"                  { return symbol(INTEGER_LITERAL, new Integer(Integer.MIN_VALUE)); }
  
  {DecIntegerLiteral}            { return symbol(INTEGER_LITERAL, new Integer(yytext())); }
  {DecLongLiteral}               { return symbol(INTEGER_LITERAL, new Long(yytext().substring(0,yylength()-1))); }
  
  {DoubleLiteral}                { return symbol(FLOATING_POINT_LITERAL, new Double(yytext())); }
  {DoubleLiteral}[dD]            { return symbol(FLOATING_POINT_LITERAL, new Double(yytext().substring(0,yylength()-1))); }
  
  /* comments */
  {Comment}                      { /* ignore */ }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }

  /* identifiers */ 
  {Identifier}                   { return symbol(IDENTIFIER, yytext()); }  
	 
}