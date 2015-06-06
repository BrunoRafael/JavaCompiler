//.flex

import java_cup.runtime.*;
%%


%class JavaScanner
%line
%column
%cup

%{
	StringBuffer buffer = new StringBuffer();
	
	private Symbol symbol(int type){
		return new Symbol(type, yyline, yycolumn);
	}
	private Symbol symbol(int type, Object value){
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

character = [a-zA-Z]
digits = [0-9]


%%

"=" {System.out.print(" = "); return new Symbol(sym.EQ)}
";" {System.out.print(" ; "); return new Symbol(sym.)}
"}" {System.out.print(" } ");}
"{"	{System.out.print(" { ");}
"("	{System.out.print(" ( ");}
")"	{System.out.print(" ) ");}
"do" {System.out.print(" do "); return Symbol()}
"while"
"for"
"if"
"switch"
"class"
"int"
"float"
"native"
"synchronized"


