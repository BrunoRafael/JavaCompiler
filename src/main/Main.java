package main;

import parser.ParserCup;
import scanner.Lexer;


public class Main {
    public static void main(String[] args) {
        try {
            for (int i = 0; i < args.length; i++) {
                System.out.println("Program " + args[i]);
                Lexer scanner = new Lexer(
                        new java.io.FileReader(args[i]));
                ParserCup p = new ParserCup(scanner);
                p.parse();
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }
}
