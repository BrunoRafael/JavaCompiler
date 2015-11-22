package scanner;

//IMPORTS
import java.io.IOException;
import parser.Sym;
import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;
import java_cup.runtime.Symbol;
import com.sun.org.apache.xalan.internal.xsltc.compiler.sym;

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
    
    public String yyPoint(){
    	return "Line : " + (yyline); 
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

Newline    = \r | \n | \r\n
Whitespace = [ \t\f] | {Newline}
Number     = [0-9]+

DecIntegerLiteral = 0 | [1-9][0-9]*
DecLongLiteral    = {DecIntegerLiteral} [lL]

HexIntegerLiteral = 0 [xX] 0* {HexDigit} {1,8}
HexLongLiteral    = 0 [xX] 0* {HexDigit} {1,16} [lL]
HexDigit          = [0-9a-fA-F]

OctIntegerLiteral = 0+ [1-3]? {OctDigit} {1,15}
OctLongLiteral    = 0+ 1? {OctDigit} {1,21} [lL]
OctDigit          = [0-7]
       
FloatLiteral  = ({FLit1}|{FLit2}|{FLit3}) {Exponent}? [fF]

FLit1    = [0-9]+ \. [0-9]* 
FLit2    = \. [0-9]+ 
FLit3    = [0-9]+ 
Exponent = [eE] [+-]? [0-9]+


//COMENTARIOS
Comment = {TraditionalComment} | {EndOfLineComment} | {DocumentationComment}
TraditionalComment = "/*" {CommentContent} \*+ "/"
EndOfLineComment = "//" [^\r\n]* {Newline}
CommentContent = ( [^*] | \*+[^*/] )*
DocumentationComment = "/*" "*"+ [^/*] ~"*/"

//IDENTACAO
ident = ([:jletter:] | "_" ) ([:jletterdigit:] | [:jletter:] | "_" )*

%eofval{
    return symbolFactory.newSymbol("EOF",sym.EOF);
%eofval}

//STRINGS E LITERAIS
StringCharacter = [^\r\n\"\\]
SingleCharacter = [^\r\n\'\\]


%state STRING, CHARLITERAL

%%  

<YYINITIAL> {

  {Whitespace} {                              }
  {DecIntegerLiteral}            { return symbolFactory.newSymbol("INTEGER_LITERAL", INTEGER_LITERAL, new Integer(yytext())); }
  {DecLongLiteral}               { return symbolFactory.newSymbol("INTEGER_LITERAL_LONG", INTEGER_LITERAL_LONG, new Long(yytext().substring(0,yylength()-1))); }
  {HexIntegerLiteral}            { return symbolFactory.newSymbol("HEX_LITERAL", HEX_LITERAL, new Integer((int) parseLong(2, yylength(), 16))); }
  {HexLongLiteral}               { return symbolFactory.newSymbol("HEX_LITERAL_LONG", HEX_LITERAL_LONG, new Long(parseLong(2, yylength()-1, 16))); }
  {OctIntegerLiteral}            { return symbolFactory.newSymbol("OCT_LITERAL", OCT_LITERAL, new Integer((int) parseLong(0, yylength(), 8))); }  
  {OctLongLiteral}               { return symbolFactory.newSymbol("OCT_LITERAL_LONG", OCT_LITERAL_LONG, new Long(parseLong(0, yylength()-1, 8))); }
  {FloatLiteral}                 { return symbolFactory.newSymbol("FLOAT_LITERAL", FLOAT_LITERAL, new Float(yytext().substring(0,yylength()-1))); }
  "<<"         { return symbolFactory.newSymbol("LSHIFT", LSHIFT); }
  ">>"         { return symbolFactory.newSymbol("RSHIFT", RSHIFT); }
  ">>="        { return symbolFactory.newSymbol("RSHIFTEQ", RSHIFTEQ); }
  ">>>"        { return symbolFactory.newSymbol("SSHIFT", SSHIFT); }
  "~"          { return symbolFactory.newSymbol("COMP", COMP); }
  ";"          { return symbolFactory.newSymbol("SEMI", SEMI); }
  "."          { return symbolFactory.newSymbol("POINT", POINT); }
  "*"          { return symbolFactory.newSymbol("MULT", MULT); }
  ","          { return symbolFactory.newSymbol("COMMA", COMMA); }
  "("          { return symbolFactory.newSymbol("LPAREN", LPAREN); }
  ")"          { return symbolFactory.newSymbol("RPAREN", RPAREN); }
  "{"          { return symbolFactory.newSymbol("LBRACE", LBRACE); }
  "}"          { return symbolFactory.newSymbol("RBRACE", RBRACE); }
  "["          { return symbolFactory.newSymbol("LBRACK", LBRACK); }
  "]"          { return symbolFactory.newSymbol("RBRACK", RBRACK); }
  "="          { return symbolFactory.newSymbol("EQ", EQ); }
  "-"          { return symbolFactory.newSymbol("MINUS", MINUS); }
  "--"         { return symbolFactory.newSymbol("MINUSMINUS", MINUSMINUS); }
  "+"          { return symbolFactory.newSymbol("PLUS", PLUS); }
  "++"         { return symbolFactory.newSymbol("PLUSPLUS", PLUSPLUS); }
  "+="         { return symbolFactory.newSymbol("PLUSEQ", PLUSEQ); }
  "-="         { return symbolFactory.newSymbol("MINUSEQ", MINUSEQ); }
  "*="         { return symbolFactory.newSymbol("MULTEQ", MULTEQ); }
  "/"          { return symbolFactory.newSymbol("DIV", DIV); }
  "/="         { return symbolFactory.newSymbol("DIVEQ", DIVEQ); }
  "%"          { return symbolFactory.newSymbol("MOD", MOD); }
  "%="         { return symbolFactory.newSymbol("MODEQ", MODEQ); }
  ">"          { return symbolFactory.newSymbol("GT", GT); }
  "<"          { return symbolFactory.newSymbol("LT", LT); }
  ">="         { return symbolFactory.newSymbol("GTEQ", GTEQ); }
  "<="         { return symbolFactory.newSymbol("LTEQ", LTEQ); }
  "=="         { return symbolFactory.newSymbol("EQEQ", EQEQ); }
  "!="         { return symbolFactory.newSymbol("NOTEQ", NOTEQ); }
  "!"          { return symbolFactory.newSymbol("NOT", NOT); }
  "?"          { return symbolFactory.newSymbol("QUESTION", QUESTION); }
  ":"          { return symbolFactory.newSymbol("COLON", COLON); }
  "|"          { return symbolFactory.newSymbol("OR", OR); }
  "||"         { return symbolFactory.newSymbol("OROR", OROR); }
  "|="         { return symbolFactory.newSymbol("OREQ", OREQ); }
  "||="        { return symbolFactory.newSymbol("OROREQ", OROREQ); }
  "^"          { return symbolFactory.newSymbol("XOR", XOR); }
  "^="         { return symbolFactory.newSymbol("XOREQ", XOREQ); }
  "new"        { return symbolFactory.newSymbol("NEW", NEW); }
  "package"    { return symbolFactory.newSymbol("PACKAGE", PACKAGE); }
  "import"     { return symbolFactory.newSymbol("IMPORT", IMPORT); }
  "class"      { return symbolFactory.newSymbol("CLASS", CLASS); }
  "public"     { return symbolFactory.newSymbol("PUBLIC", PUBLIC); }
  "protected"  { return symbolFactory.newSymbol("PROTECTED", PROTECTED); }
  "private"    { return symbolFactory.newSymbol("PRIVATE", PRIVATE); }
  "static"     { return symbolFactory.newSymbol("STATIC", STATIC); }
  "abstract"   { return symbolFactory.newSymbol("ABSTRACT", ABSTRACT); }
  "final"      { return symbolFactory.newSymbol("FINAL", FINAL); }
  "native"     { return symbolFactory.newSymbol("NATIVE", NATIVE); }
  "synchronized"     { return symbolFactory.newSymbol("SYNCHRONIZED", SYNCHRONIZED); }
  "transient"  { return symbolFactory.newSymbol("TRANSIENT", TRANSIENT); }
  "volatile"   { return symbolFactory.newSymbol("VOLATILE", VOLATILE); }
  "extends"    { return symbolFactory.newSymbol("EXTENDS", EXTENDS); }
  "implements" { return symbolFactory.newSymbol("IMPLEMENTS", IMPLEMENTS); }
  "boolean"    { return symbolFactory.newSymbol("BOOLEAN", BOOLEAN); }
  "byte"       { return symbolFactory.newSymbol("BYTE", BYTE); } 
  "char"       { return symbolFactory.newSymbol("CHAR", CHAR); }
  "short"      { return symbolFactory.newSymbol("SHORT", SHORT); } 
  "int"        { return symbolFactory.newSymbol("INT", INT); }
  "float"      { return symbolFactory.newSymbol("FLOAT", FLOAT); } 
  "long"       { return symbolFactory.newSymbol("LONG", LONG); } 
  "true"       { return symbolFactory.newSymbol("TRUE", TRUE); }
  "false"      { return symbolFactory.newSymbol("FALSE", FALSE); }
  "null"       { return symbolFactory.newSymbol("NULL", NULL); }
  "super"      { return symbolFactory.newSymbol("SUPER", SUPER); }
  "this"       { return symbolFactory.newSymbol("THIS", THIS); }
  "instanceof" { return symbolFactory.newSymbol("INSTANCEOF", INSTANCEOF); }
  "ampersand"  { return symbolFactory.newSymbol("AMPERSAND", AMPERSAND); }
  "ampersand=" { return symbolFactory.newSymbol("AMPERSANDEQ", AMPERSANDEQ); }
  "return" 	   { return symbolFactory.newSymbol("RETURN", RETURN); }
  "throw" 	   { return symbolFactory.newSymbol("THROW", THROW); }
  "break" 	   { return symbolFactory.newSymbol("BREAK", BREAK); }
  "continue"   { return symbolFactory.newSymbol("CONTINUE", CONTINUE); }
  "if"         { return symbolFactory.newSymbol("IF", IF); }
  "else"       { return symbolFactory.newSymbol("ELSE", ELSE); }
  "do"         { return symbolFactory.newSymbol("DO", DO); }
  "while"      { return symbolFactory.newSymbol("WHILE", WHILE); }
  "for"        { return symbolFactory.newSymbol("FOR", FOR); }
  "try"        { return symbolFactory.newSymbol("TRY", TRY); }
  "catch"      { return symbolFactory.newSymbol("CATCH", CATCH); }
  "finally"    { return symbolFactory.newSymbol("FINALLY", FINALLY); }
  "switch"     { return symbolFactory.newSymbol("SWITCH", SWITCH); }
  "case"       { return symbolFactory.newSymbol("CASE", CASE); }
  "default"    { return symbolFactory.newSymbol("DEFAULT", DEFAULT); }
  "interface"  { return symbolFactory.newSymbol("INTERFACE", INTERFACE); }
  {Comment}    { /* ignore */ }
  {ident}      { return symbolFactory.newSymbol("IDENTIFIER", IDENTIFIER, yytext().toString()); }
  
  /* string literal */
  \"                             { yybegin(STRING); string.setLength(0); }

  /* character literal */
  \'                             { yybegin(CHARLITERAL); }
  
}

<STRING> {
  \"                             { yybegin(YYINITIAL); return symbolFactory.newSymbol("STRING_LITERAL", STRING_LITERAL, string.toString()); }
  
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
}

<CHARLITERAL> {
  {SingleCharacter}\'            { yybegin(YYINITIAL); return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character(yytext().charAt(0))); }
  
  /* escape sequences */
  "\\b"\'                        { yybegin(YYINITIAL); return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character('\b'));}
  "\\t"\'                        { yybegin(YYINITIAL); return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character('\t'));}
  "\\n"\'                        { yybegin(YYINITIAL); return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character('\n'));}
  "\\f"\'                        { yybegin(YYINITIAL); return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character('\f'));}
  "\\r"\'                        { yybegin(YYINITIAL); return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character('\r'));}
  "\\\""\'                       { yybegin(YYINITIAL); return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character('\"'));}
  "\\'"\'                        { yybegin(YYINITIAL); return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character('\''));}
  "\\\\"\'                       { yybegin(YYINITIAL); return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character('\\')); }
  \\[0-3]?{OctDigit}?{OctDigit}\' { yybegin(YYINITIAL); 
			                              int val = Integer.parseInt(yytext().substring(1,yylength()-1),8);
			                            return symbolFactory.newSymbol("CHARACTER_LITERAL", CHARACTER_LITERAL, new Character((char)val)); }

}

//ERRO DE CARACATERE INVALIDO
{Number}{ident} { throw new RuntimeException("Variáveis não podem começar com números, erro na variável:\n"
											+yytext()); }
[^]|\n                             { throw new RuntimeException("Caractere inválido."); }
