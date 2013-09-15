import java.io.*;
import org.antlr.runtime.*;

public class Main {

    public static void main(String[] args) throws Exception {
		try{
			CharStream input = new ANTLRFileStream(args[0]);
			pascalLexer lexer = new pascalLexer(input);
			CommonTokenStream tokens = new CommonTokenStream(lexer);
			pascalParser parser = new pascalParser(tokens, "output"+File.separator+"result.py");

			parser.program();

		} catch (IOException e){
			e.printStackTrace();
		}
    }
}