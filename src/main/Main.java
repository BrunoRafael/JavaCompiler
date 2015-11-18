package main;

import java.io.File;
import java.io.FileReader;

import parser.ParserCup;
import scanner.Lexer;


public class Main {
	public static String[] files;
	public static String path = "files/";
    public static void main(String[] args) {
    	if (args.length > 0) {
			files= args;
		} else {
			File f = new File(path);
			files = f.list();
		}
        try {
            for (int i = 0; i < files.length; i++) {
                System.out.println("Program " + files[i]);
                Lexer scanner = new Lexer(new java.io.FileReader(path + files[i]));
                FileReader ff = new java.io.FileReader(path + files[i]); 
                ParserCup p = new ParserCup(scanner);
                p.parse();
                System.out.println("No errors.");
            }

        } catch (Exception e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
    }
}
