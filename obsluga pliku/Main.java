import java.io.*;
import org.antlr.runtime.*;

public class Main {

    public static void main(String[] args) throws Exception {
		try{
			CharStream input = new ANTLRFileStream(args[0]);
			pascal3gLexer lexer = new pascal3gLexer(input);
			CommonTokenStream tokens = new CommonTokenStream(lexer);
			pascal3gParser parser = new pascal3gParser(tokens, "output"+File.separator+"result.py");

			parser.program();

		} catch (IOException e){
			e.printStackTrace();
		}
    }
}