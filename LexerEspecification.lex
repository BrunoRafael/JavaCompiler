
package scanner;

import java_cup.runtime.*;
import java.io.IOException;

import parser.Sym;
import static parser.Sym.*;

%%

%class Lex

%unicode
%line
%column

%public
// %final
// %abstract

%cupsym parser.Sym
%cup
// %cupdebug

%init{
	// TODO: code that goes to constructor
%init}

%{
	private Symbol sym(int type)
	{
		return sym(type, yytext());
	}

	private Symbol sym(int type, Object value)
	{
		return new Symbol(type, yyline, yycolumn, value);
	}

	private void error()
	throws IOException
	{
		throw new IOException("illegal text at line = "+yyline+", column = "+yycolumn+", text = '"+yytext()+"'");
	}
%}

/* terminator characters */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]

Comments = {DocComment} | {PatternComment} | {LineComment}

LineComment = "//" {InputCharacter}* {LineTerminator}?
DocComment = "/*" "*"+ [^/*] ~"*/"
PatternComment = "/*" [^*] ~"*/" | "/*" "*"+ "/"

EOF			=	.
%%

{EOF}		{return sym(EOF);}

{WhiteSpace}  {/*ignore*/}

