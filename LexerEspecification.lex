package scanner;

import java.io.IOException;
import parser.Sym;
import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;
import java_cup.runtime.Symbol;

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
    public Lexer(ComplexSymbolFactory sf, java.io.InputStream is){
		this(is);
        symbolFactory = sf;
    }
	public Lexer(ComplexSymbolFactory sf, java.io.Reader reader){
		this(reader);
        symbolFactory = sf;
    }
    
    public Lexer(){};
    
    private StringBuffer sb;
    private ComplexSymbolFactory symbolFactory;
    private int csline,cscolumn;

    public Symbol symbol(String name, int code){
		return symbolFactory.newSymbol(name, code,
						new Location(yyline+1,yycolumn+1), // -yylength()
						new Location(yyline+1,yycolumn+yylength())
				);
    }
    public Symbol symbol(String name, int code, String lexem){
	return symbolFactory.newSymbol(name, code, 
						new Location(yyline+1, yycolumn +1), 
						new Location(yyline+1,yycolumn+yylength()), lexem);
    }
    
    public Symbol symbol(int code, Object lexem){
	return symbolFactory.newSymbol("Token", code, 
						new Location(yyline+1, yycolumn +1), 
						new Location(yyline+1,yycolumn+yylength()), lexem);
    }
    public String current_lexeme(){
    	int l = yyline+1;
    	int c = yycolumn+1;
    	return " (line: "+l+" , column: "+c+" , lexeme: '"+yytext()+"')";
  	}

  /** 
   * assumes correct representation of a long value for 
   * specified radix in scanner buffer from <code>start</code> 
   * to <code>end</code> 
   */
  private long parseLong(int start, int end, int radix) {
    long result = 0;
    long digit;

    for (int i = start; i < end; i++) {
      digit  = Character.digit(yycharat(i),radix);
      result*= radix;
      result+= digit;
    }

    return result;
  }

%}

/* main character classes */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment} | 
          {DocumentationComment}

TraditionalComment = "/*" [^*] ~"*/" | "/*" "*"+ "/"
EndOfLineComment = "//" {InputCharacter}* {LineTerminator}?
DocumentationComment = "/*" "*"+ [^/*] ~"*/"

/* identifiers */
Identifier = [a-zA-Z_][a-zA-Z_0-9]*

/* integer literals */
DecIntegerLiteral = 0 | [1-9][0-9]*
DecLongLiteral    = {DecIntegerLiteral} [lL]

HexIntegerLiteral = 0 [xX] 0* {HexDigit} {1,8}
HexLongLiteral    = 0 [xX] 0* {HexDigit} {1,16} [lL]
HexDigit          = [0-9a-fA-F]

OctIntegerLiteral = 0+ [1-3]? {OctDigit} {1,15}
OctLongLiteral    = 0+ 1? {OctDigit} {1,21} [lL]
OctDigit          = [0-7]
    
/* floating point literals */        
FloatLiteral  = ({FLit1}|{FLit2}|{FLit3}) {Exponent}? [fF]
DoubleLiteral = ({FLit1}|{FLit2}|{FLit3}) {Exponent}?

FLit1    = [0-9]+ \. [0-9]* 
FLit2    = \. [0-9]+ 
FLit3    = [0-9]+ 
Exponent = [eE] [+-]? [0-9]+

/* string and character literals */
StringCharacter = [^\r\n\"\\]
SingleCharacter = [^\r\n\'\\]

%state STRING, CHARLITERAL

%%

<YYINITIAL> {

  /* keywords */
  "abstract"                     { return symbol("ABSTRACT", ABSTRACT); }
  "boolean"                      { return symbol("BOOLEAN", BOOLEAN); }
  "break"                        { return symbol("BREAK", BREAK); }
  "byte"                         { return symbol("BYTE", BYTE); }
  "case"                         { return symbol("CASE", CASE); }
  "catch"                        { return symbol("CATCH", CATCH); }
  "char"                         { return symbol("CHAR", CHAR); }
  "class"                        { return symbol("CLASS", CLASS); }
  "continue"                     { return symbol("CONTINUE", CONTINUE); }
  "do"                           { return symbol("DO", DO); }
  "double"                       { return symbol("DOUBLE", DOUBLE); }
  "else"                         { return symbol("ELSE", ELSE); }
  "extends"                      { return symbol("EXTENDS", EXTENDS); }
  "final"                        { return symbol("FINAL", FINAL); }
  "finally"                      { return symbol("FINALLY", FINALLY); }
  "float"                        { return symbol("FLOAT", FLOAT); }
  "for"                          { return symbol("FOR", FOR); }
  "default"                      { return symbol("DEFAULT", DEFAULT); }
  "implements"                   { return symbol("IMPLEMENTS", IMPLEMENTS); }
  "import"                       { return symbol("IMPORT", IMPORT); }
  "instanceof"                   { return symbol("INSTANCEOF", INSTANCEOF); }
  "int"                          { return symbol("INT", INT); }
  "interface"                    { return symbol("INTERFACE", INTERFACE); }
  "long"                         { return symbol("LONG", LONG); }
  "native"                       { return symbol("NATIVE", NATIVE); }
  "new"                          { return symbol("NEW", NEW); }
  "if"                           { return symbol("IF", IF); }
  "public"                       { return symbol("PUBLIC", PUBLIC); }
  "short"                        { return symbol("SHORT", SHORT); }
  "super"                        { return symbol("SUPER", SUPER); }
  "switch"                       { return symbol("SWITCH", SWITCH); }
  "synchronized"                 { return symbol("SYNCHRONIZED", SYNCHRONIZED); }
  "package"                      { return symbol("PACKAGE", PACKAGE); }
  "private"                      { return symbol("PRIVATE", PRIVATE); }
  "protected"                    { return symbol("PROTECTED", PROTECTED); }
  "transient"                    { return symbol("TRANSIENT", TRANSIENT); }
  "return"                       { return symbol("RETURN", RETURN); }
  "void"                         { return symbol("VOID", VOID); }
  "static"                       { return symbol("STATIC", STATIC); }
  "while"                        { return symbol("WHILE", WHILE); }
  "this"                         { return symbol("THIS", THIS); }
  "throw"                        { return symbol("THROW", THROW); }
  "throws"                       { return symbol("THROWS", THROWS); }
  "try"                          { return symbol("TRY",TRY); }
  
  /* boolean literals */
  "true"                         { return symbol("TRUE", BOOLEAN_LITERAL); }
  "false"                        { return symbol("FALSE", BOOLEAN_LITERAL); }
  
  /* null literal */
  "null"                         { return symbol("NULL", NULL_LITERAL); }
  
  
  /* separators */
  "("                            { return symbol("(", LPAREN); }
  ")"                            { return symbol(")", RPAREN); }
  "{"                            { return symbol("{", LBRACE); }
  "}"                            { return symbol("}", RBRACE); }
  "["                            { return symbol("[", LBRACK); }
  "]"                            { return symbol("]", RBRACK); }
  ";"                            { return symbol(";", SEMICOLON); }
  ","                            { return symbol(",", COMMA); }
  "."                            { return symbol(".", DOT); }
  
  /* operators */
  "="                            { return symbol("=", EQ); }
  ">"                            { return symbol(">", GT); }
  "<"                            { return symbol("<", LT); }
  "!"                            { return symbol("!", NOT); }
  "~"                            { return symbol("~", COMP); }
  "?"                            { return symbol("?", QUESTION); }
  ":"                            { return symbol(":", COLON); }
  "=="                           { return symbol("==", EQEQ); }
  "<="                           { return symbol("<=", LTEQ); }
  ">="                           { return symbol(">=", GTEQ); }
  "!="                           { return symbol("!=", NOTEQ); }
  "&&"                           { return symbol("&&", ANDAND); }
  "||"                           { return symbol("||", OROR); }
  "++"                           { return symbol("++", PLUSPLUS); }
  "--"                           { return symbol("--", MINUSMINUS); }
  "+"                            { return symbol("+", PLUS); }
  "-"                            { return symbol("-", MINUS); }
  "*"                            { return symbol("*", MULT); }
  "/"                            { return symbol("/", DIV); }
  "&"                            { return symbol("&", AND); }
  "|"                            { return symbol("|", OR); }
  "^"                            { return symbol("^", XOR); }
  "%"                            { return symbol("%", MOD); }
  "<<"                           { return symbol("<<", LSHIFT); }
  ">>"                           { return symbol(">>", RSHIFT); }
  ">>>"                          { return symbol(">>>", URSHIFT); }
  "+="                           { return symbol("+=", PLUSEQ); }
  "-="                           { return symbol("-=", MINUSEQ); }
  "*="                           { return symbol("*=", MULTEQ); }
  "/="                           { return symbol("/=", DIVEQ); }
  "&="                           { return symbol("&=", ANDEQ); }
  "|="                           { return symbol("|=", OREQ); }
  "^="                           { return symbol("^=", XOREQ); }
  "%="                           { return symbol("%=", MODEQ); }
  "<<="                          { return symbol("<<=", LSHIFTEQ); }
  ">>="                          { return symbol(">>=", RSHIFTEQ); }
  ">>>="                         { return symbol(">>>=", URSHIFTEQ); }
  
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
  
  {HexIntegerLiteral}            { return symbol(INTEGER_LITERAL, new Integer((int) parseLong(2, yylength(), 16))); }
  {HexLongLiteral}               { return symbol(INTEGER_LITERAL, new Long(parseLong(2, yylength()-1, 16))); }
 
  {OctIntegerLiteral}            { return symbol(INTEGER_LITERAL, new Integer((int) parseLong(0, yylength(), 8))); }  
  {OctLongLiteral}               { return symbol(INTEGER_LITERAL, new Long(parseLong(0, yylength()-1, 8))); }
  
  {FloatLiteral}                 { return symbol(FLOATING_POINT_LITERAL, new Float(yytext().substring(0,yylength()-1))); }
  {DoubleLiteral}                { return symbol(FLOATING_POINT_LITERAL, new Double(yytext())); }
  {DoubleLiteral}[dD]            { return symbol(FLOATING_POINT_LITERAL, new Double(yytext().substring(0,yylength()-1))); }
  
  /* comments */
  {Comment}                      { /* ignore */ }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }

  /* identifiers */ 
  {Identifier}                   { return symbol(IDENTIFIER, yytext()); } 
}

<STRING> {
  \"                             { yybegin(YYINITIAL); return symbol(Sym.STRING_LITERAL, string.toString()); }
  
  {StringCharacter}+             { string.append( yytext() ); }
  
  /* escape sequences */
  "\\b"                          { string.append( '\b' ); }
  "\\t"                          { string.append( '\t' ); }
  "\\n"                          { string.append( '\n' ); }
  "\\f"                          { string.append( '\f' ); }
  "\\r"                          { string.append( '\r' ); }
  "\\\""                         { string.append( '\"' ); }
  "\\'"                          { string.append( '\'' ); }
  "\\\\"                         { string.append( '\\' ); }
  \\[0-3]?{OctDigit}?{OctDigit}  { char val = (char) Integer.parseInt(yytext().substring(1),8);
                        				   string.append( val ); }
  
  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
  {LineTerminator}               { throw new RuntimeException("Unterminated string at end of line"); }
  
}

<CHARLITERAL> {
  {SingleCharacter}\'            { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, yytext().charAt(0)); }
  
  /* escape sequences */
  "\\b"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\b');}
  "\\t"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\t');}
  "\\n"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\n');}
  "\\f"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\f');}
  "\\r"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\r');}
  "\\\""\'                       { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\"');}
  "\\'"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\'');}
  "\\\\"\'                       { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\\'); }
  \\[0-3]?{OctDigit}?{OctDigit}\' { yybegin(YYINITIAL); 
			                              int val = Integer.parseInt(yytext().substring(1,yylength()-1),8);
			                            return symbol(CHARACTER_LITERAL, (char)val); } 
  
  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
  {LineTerminator}               { throw new RuntimeException("Unterminated character literal at end of line"); }
}

/* error fallback */
.|\n                             { throw new Error(" Nao permitido <"+yytext()+">; line: " + yyline + " column: " + yycolumn); }
<<EOF>>                          { return symbol("EOF", EOF); }
