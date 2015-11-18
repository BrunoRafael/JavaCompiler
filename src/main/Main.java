package main;

import java.io.File;

import parser.ParserCup;
import scanner.Lexer;


public class Main {
	public static String file;
	public static String path = "files/";
    public static void main(String[] args) {
    	String[] files;
    	if (args.length > 0) {
			files= args;
		} else {
			File f = new File(path);
			files = f.list();
		}
        try {
            for (int i = 0; i < files.length; i++) {
            	file = path + files[i];
                new ParserCup().parse();
                System.out.println("No errors.");
            }

        } catch (Exception e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
    }
}
