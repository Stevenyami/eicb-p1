/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.parsing;

import mavlc.errors.SyntaxError;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;

import java.util.*;

import static mavlc.parsing.Token.TokenType.*;
import static mavlc.syntax.expression.Compare.Comparison.*;

/**
 * EiCB group number: 040
 * Names and matriculation numbers of all group members:
 * Julian von Hammel (2972165)
 * Nil Sila Ulucan (2378251)
 * Nyami Gochui Steve Jordi (2756527)
 */

/**
 * A recursive-descent parser for MAVL.
 */
public final class Parser {
	
	private final Deque<Token> tokens;
	private Token currentToken;
	
	/**
	 * @param tokens A token stream that was produced by the {@link Scanner}.
	 */
	public Parser(Deque<Token> tokens) {
		this.tokens = tokens;
		currentToken = tokens.poll();
	}
	
	/**
	 * Parses the MAVL grammar's start symbol, Module.
	 *
	 * @return A {@link Module} node that is the root of the AST representing the tokenized input program.
	 * @throws SyntaxError to indicate that an unexpected token was encountered.
	 */
	public Module parse() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Function> functions = new ArrayList<>();
		List<RecordTypeDeclaration> records = new ArrayList<>();
		while(currentToken.type != EOF) {
			switch(currentToken.type) {
				case FUNCTION:
					functions.add(parseFunction());
					break;
				case RECORD:
					records.add(parseRecordTypeDeclaration());
					break;
				default:
					throw new SyntaxError(currentToken, FUNCTION, RECORD);
			}
		}
		return new Module(location, functions, records);
	}
	
	private String accept(Token.TokenType type) {
		Token t = currentToken;
		if(t.type != type)
			throw new SyntaxError(t, type);
		acceptIt();
		return t.spelling;
	}
	
	private void acceptIt() {
		currentToken = tokens.poll();
		if(currentToken == null || currentToken.type == ERROR)
			throw new SyntaxError(currentToken != null ? currentToken : new Token(EOF, null, -1, -1));
	}
	
	private Function parseFunction() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FUNCTION);
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		List<FormalParameter> parameters = new ArrayList<>();
		List<Statement> body = new ArrayList<>();
		
		accept(LPAREN);
		if(currentToken.type != RPAREN) {
			parameters.add(parseFormalParameter());
			while(currentToken.type != RPAREN) {
				accept(COMMA);
				parameters.add(parseFormalParameter());
			}
		}
		accept(RPAREN);
		
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			body.add(parseStatement());
		accept(RBRACE);
		
		return new Function(location, name, typeSpecifier, parameters, body);
	}
	
	private FormalParameter parseFormalParameter() {
		SourceLocation location = currentToken.sourceLocation;
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		return new FormalParameter(location, name, typeSpecifier);
	}
	
	private RecordTypeDeclaration parseRecordTypeDeclaration() {
		SourceLocation location = currentToken.sourceLocation; // damit das Programm besser Fehler meldung werfen kann. Man weisst genau wo er den Fehler suchen muss
		
		accept(RECORD); // @
		String name = accept(ID); // ID
		accept(LBRACE); // {
		List<RecordElementDeclaration> elements = new ArrayList<>();
		// no empty records allowed
		elements.add(parseRecordElementDeclaration());
		while(currentToken.type != RBRACE) {
			elements.add(parseRecordElementDeclaration());
		}
		accept(RBRACE);
		
		return new RecordTypeDeclaration(location, name, elements);
	}
	
	private RecordElementDeclaration parseRecordElementDeclaration() {
		SourceLocation location = currentToken.sourceLocation; // damit das Programm besser Fehler meldung werfen kann. Man weisst genau wo er den Fehler suchen muss
		
		boolean isVariable;
		switch(currentToken.type) {
			case VAL:
				acceptIt();
				isVariable = false;
				break;
			case VAR:
				acceptIt();
				isVariable = true;
				break;
			default:
				throw new SyntaxError(currentToken, VAL, VAR);
		}

		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		accept(SEMICOLON);

		return new RecordElementDeclaration(location, isVariable, typeSpecifier, name);
	}
	
	private IteratorDeclaration parseIteratorDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		boolean variable = currentToken.type == VAR; // Zuweisung von VAR, da VAl eine Konstante ist
		if (variable || currentToken.type == VAL){
			acceptIt();
		}else {
			throw  new SyntaxError(currentToken,VAR, VAL);
		}
		TypeSpecifier type = parseTypeSpecifier();
		String ident = accept(ID);
		return new IteratorDeclaration(location, ident, type, variable);
	}
	
	private TypeSpecifier parseTypeSpecifier() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean vector = false;
		switch(currentToken.type) {
			case INT:
				acceptIt();
				return new IntTypeSpecifier(location);
			case FLOAT:
				acceptIt();
				return new FloatTypeSpecifier(location);
			case BOOL:
				acceptIt();
				return new BoolTypeSpecifier(location);
			case VOID:
				acceptIt();
				return new VoidTypeSpecifier(location);
			case STRING:
				acceptIt();
				return new StringTypeSpecifier(location);
			case VECTOR:
				accept(VECTOR);
				vector = true;
				break;
			case MATRIX:
				accept(MATRIX);
				break;
			case ID:
				String name = accept(ID);
				return new RecordTypeSpecifier(location, name);
			default:
				throw new SyntaxError(currentToken, INT, FLOAT, BOOL, VOID, STRING, VECTOR, MATRIX, ID);
		}
		
		accept(LANGLE);
		TypeSpecifier subtype;
		switch(currentToken.type) {
			case INT:
				subtype = new IntTypeSpecifier(currentToken.sourceLocation);
				break;
			case FLOAT:
				subtype = new FloatTypeSpecifier(currentToken.sourceLocation);
				break;
			default:
				throw new SyntaxError(currentToken, INT, FLOAT);
		}
		acceptIt();
		accept(RANGLE);
		accept(LBRACKET);
		Expression x = parseExpr();
		accept(RBRACKET);
		
		if(vector)
			return new VectorTypeSpecifier(location, subtype, x);
		
		accept(LBRACKET);
		Expression y = parseExpr();
		accept(RBRACKET);
		
		return new MatrixTypeSpecifier(location, subtype, x, y);
	}
	
	private Statement parseStatement() {
		switch(currentToken.type) {
			case VAL:
				return parseValueDef();
			case VAR:
				return parseVarDecl();
			case RETURN:
				return parseReturn();
			case ID:
				return parseAssignOrCall();
			case FOR:
				return parseFor();
			case FOREACH:
				return parseForEach();
			case IF:
				return parseIf();
			case SWITCH:
				return parseSwitch();
			case LBRACE:
				return parseCompound();
			default:
				throw new SyntaxError(currentToken, VAL, VAR, RETURN, ID, FOR, FOREACH, IF, SWITCH, LBRACE);
		}
	}
	
	private ValueDefinition parseValueDef() {
		SourceLocation location = currentToken.sourceLocation;  // damit das Programm besser Fehler meldung werfen kann. Man weisst genau wo er den Fehler suchen muss
		accept(VAL);
		// implementierung der Logik der Grammatik
		TypeSpecifier typespecifier = parseTypeSpecifier();
		String name = accept(ID);// ID
		accept(ASSIGN);
		Expression value = parseExpr(); // Expression
		accept(SEMICOLON);
		return new ValueDefinition(location, typespecifier, name, value);
	}
	
	private VariableDeclaration parseVarDecl() {
		SourceLocation location = currentToken.sourceLocation;  // damit das Programm besser Fehler meldung werfen kann. Man weisst genau wo er den Fehler suchen muss
		// codierung der Logik der Grammatik
		accept(VAR);
		TypeSpecifier typespecifier = parseTypeSpecifier();
		String name = accept(ID);
		accept(SEMICOLON);
		return new VariableDeclaration(location, typespecifier, name);
	}
	
	private ReturnStatement parseReturn() {
		SourceLocation location = currentToken.sourceLocation;  // damit das Programm besser Fehler meldung werfen kann. Man weisst genau wo er den Fehler suchen muss
		// Implementierung der Logik der Grammatik
		accept(RETURN);
		Expression returnValue = parseExpr();
		accept(SEMICOLON);
		return  new ReturnStatement(location, returnValue);
	}
	
	private Statement parseAssignOrCall() {
		Statement result;
		SourceLocation location = currentToken.sourceLocation; // damit er besser Fehler meldung werfen kann. Man weisst genau wo er den Fehler suchen muss
		// Codierung der Grammatik
		String ident = accept(ID);
		if (currentToken.type == LBRACKET || currentToken.type == AT || currentToken.type == ASSIGN ){
			result = parseAssign(ident, location);
			accept(SEMICOLON);
			return result;
		} else if ( currentToken.type == LPAREN){
			CallExpression a = parseCall(ident, location);
			accept(SEMICOLON);
			return new CallStatement(location, a); // muss noch ein CallStatement enstehen da der eigentlich statement kein Rückgabewert hat aber die Methode schon
		}else {
			throw new SyntaxError(currentToken, LBRACKET, AT, ASSIGN, LPAREN);
		}
	}
	
	private VariableAssignment parseAssign(String name, SourceLocation location) {
		LeftHandIdentifier lhi;  // LeftHandIdentifier: Record / Matrix / Vektor-LhsIdentifier
		if(currentToken.type == LBRACKET){  // codierung 1.expression
			accept(LBRACKET);
			Expression value1 = parseExpr();
			accept(RBRACKET);
			if(currentToken.type == LBRACKET){ // codierung 2.expression: in dem Fall eine Matrix
				acceptIt();
				Expression value2 = parseExpr();
				accept(RBRACKET);
				lhi = new MatrixLhsIdentifier(location, name, value1,value2);
			}else { // sonst trigen wir ein Vektor
				lhi = new VectorLhsIdentifier(location, name, value1);
			}
		}else if (currentToken.type == AT){ // Record-codierung '@' ID: wenn es um keine Matrix und keinen Vektor handelt
			acceptIt();
			String Ident = accept(ID);
			lhi = new RecordLhsIdentifier(location, name,Ident);
		}else { // Ansonsten ist lhi einfach ein LHIdentifier
			lhi = new LeftHandIdentifier(location, name);
		}
		accept(ASSIGN);
		Expression res = parseExpr(); // die zugewiesene Expression
		return  new VariableAssignment(location, lhi, res);
	}
	
	private CallExpression parseCall(String name, SourceLocation location) {
		List<Expression> lst = new ArrayList<>();
		accept(LPAREN);
		if( currentToken.type != RPAREN ){
			 lst.add(parseExpr());
			while (currentToken.type == COMMA){
				acceptIt();
				lst.add(parseExpr());
			}
		}
		accept(RPAREN);
		return new CallExpression(location, name,lst);
	}
	
	private ForLoop parseFor() {
		SourceLocation location = currentToken.sourceLocation;  // damit das Programm besser Fehler meldung werfen kann. Man weisst genau wo er den Fehler suchen muss
		// Kodierung der Logik
		accept(FOR);
		accept(LPAREN);
		String ident1 = accept(ID);
		accept(ASSIGN);
		Expression expr1 = parseExpr();
		accept(SEMICOLON);
		Expression expr2 = parseExpr();
		accept(SEMICOLON);
		String ident2 = accept(ID);
		accept(ASSIGN);
		Expression expr3 = parseExpr();
		accept(RPAREN);
		Statement then = parseStatement();
		return new ForLoop(location, ident1, expr1, expr2, ident2, expr3, then);
	}
	
	private ForEachLoop parseForEach() {
		SourceLocation location = currentToken.sourceLocation;
		// Kodierung der Logik
		accept(FOREACH);
		accept(LPAREN);
		IteratorDeclaration expr1 = parseIteratorDeclaration();
		accept(COLON);
		Expression expr2 = parseExpr();
		accept(RPAREN);
		Statement then = parseStatement();
		return new ForEachLoop(location, expr1, expr2, then);
	}
	
	private IfStatement parseIf() {
		SourceLocation location = currentToken.sourceLocation;
		// Kodierung der logik
		accept(IF);
		accept(LPAREN);
		Expression test = parseExpr();
		accept(RPAREN);
		Statement then = parseStatement();
		if(currentToken.type == ELSE) {
			acceptIt();
			return new IfStatement(location, test, then, parseStatement());
		}
		return new IfStatement(location, test, then);
	}
	
	private SwitchStatement parseSwitch() {
		SourceLocation location = currentToken.sourceLocation;
		List<Case> cases = new ArrayList<>();
		List<Default> defaults = new ArrayList<>();
		accept(SWITCH);
		accept(LPAREN);
		Expression expr = parseExpr();
		accept(RPAREN);
		accept(LBRACE);
		while(currentToken.type == CASE || currentToken.type == DEFAULT){
			if ( currentToken.type == CASE){
				cases.add(parseCase());
			}else{
				defaults.add(parseDefault());
			}
		}
		accept(RBRACE);
		return new SwitchStatement(location,expr, cases, defaults);
	}
	
	private Case parseCase() {
		SourceLocation location = currentToken.sourceLocation;
		accept(CASE);
		Expression condition = parseExpr();
		accept(COLON);
		Statement stmt = parseStatement();
		return new Case(location, condition, stmt);
	}

	private Default parseDefault() {
		SourceLocation location = currentToken.sourceLocation;
		accept(DEFAULT);
		accept(COLON);
		Statement stmt = parseStatement();
		return new Default(location, stmt);
	}
	
	private CompoundStatement parseCompound() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Statement> statements = new ArrayList<>();
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			statements.add(parseStatement());
		accept(RBRACE);
		
		return new CompoundStatement(location, statements);
	}
	
	private Expression parseExpr() {
		return parseSelect();
	}
	
	private Expression parseSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression cond = parseOr();
		if(currentToken.type == QMARK) {
			acceptIt();
			Expression trueCase = parseOr();
			accept(COLON);
			Expression falseCase = parseOr();
			return new SelectExpression(location, cond, trueCase, falseCase);
		}
		return cond;
	}
	
	private Expression parseOr() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAnd();
		while(currentToken.type == OR) {
			acceptIt();
			x = new Or(location, x, parseAnd());
		}
		return x;
	}
	
	private Expression parseAnd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseNot();
		while(currentToken.type == AND) {
			acceptIt();
			x = new And(location, x, parseNot());
		}
		return x;
	}
	
	private Expression parseNot() {
		//Anwendung von der Methode compare() in der grammatik: Dann ist der zweite Parameter die Methode parseCompare()
		SourceLocation location = currentToken.sourceLocation;
		if(currentToken.type == NOT) {
			acceptIt();
			return new Not(location, parseCompare());
		}
		return parseCompare();  // liefert einfach den Aufruf von der entsprechenden Methode zurück
	}
	
	private Expression parseCompare() {
		SourceLocation location = currentToken.sourceLocation;
		Expression compErgeb = parseAddSub();
		// Kodierung der Logik
		while ((currentToken.type == RANGLE) || (currentToken.type == LANGLE) || (currentToken.type == CMPLE) || (currentToken.type == CMPGE)
		       || (currentToken.type == CMPEQ) || (currentToken.type== CMPNE)){
			if (currentToken.type == RANGLE){
				acceptIt();
				compErgeb = new Compare (location, compErgeb, parseAddSub(), GREATER);
			} else if (currentToken.type == LANGLE) {
				acceptIt();
				compErgeb = new Compare(location, compErgeb, parseAddSub(), LESS);
			} else if (currentToken.type == CMPLE) {
				acceptIt();
				compErgeb = new Compare(location, compErgeb , parseAddSub(), LESS_EQUAL);
			} else if (currentToken.type == CMPGE) {
				acceptIt();
				compErgeb = new Compare(location, compErgeb, parseAddSub(), GREATER_EQUAL);
			} else if (currentToken.type == CMPEQ) {
				acceptIt();
				compErgeb = new Compare(location, compErgeb, parseAddSub(), EQUAL);
			}else {
				acceptIt();
				compErgeb = new Compare(location, compErgeb, parseAddSub(), NOT_EQUAL);
			}
		}
		 return compErgeb;

	}
	
	private Expression parseAddSub() {
		SourceLocation location = currentToken.sourceLocation;
		Expression Expr= parseMulDiv();
		// die implementierung muss so lange einen Operand( + / - ) steht geführt werden (wegen der Stern "*" in der Grammatik )
		while (currentToken.type == ADD || currentToken.type == SUB){
			if (currentToken.type == ADD){
				acceptIt();
				Expr = new Addition(location, Expr, parseMulDiv());
			} else {
				acceptIt();
				Expr = new Subtraction(location, Expr, parseMulDiv());
			}
		}
		return Expr;
	}

	private Expression parseMulDiv() {
		SourceLocation location = currentToken.sourceLocation;
		Expression muldivErgeb = parseUnaryMinus();
		// die Operation muss so lange einen Operand( * |  / ) steht geführt werden (wegen der Stern "*" in der Grammatik )
		while (currentToken.type == MULT || currentToken.type == DIV){
			if (currentToken.type == MULT){
				acceptIt();
				muldivErgeb = new Multiplication(location, muldivErgeb,parseUnaryMinus());
			} else {
				acceptIt();
				muldivErgeb = new Division(location, muldivErgeb, parseUnaryMinus());
			}
		}
		return muldivErgeb;
	}
	
	private Expression parseUnaryMinus() {
		// UnaryMinus benutz die Methode exponentation in der Grammatik, deswegen brauchen wir parseExponentiation() in der implementierung
		SourceLocation location = currentToken.sourceLocation;
		if(currentToken.type == SUB) {
			acceptIt();
			return new UnaryMinus(location, parseExponentiation());
		}
		return parseExponentiation(); // und liefert einfach den Aufruf von der entsprechenden Methode zurück
	}
	
	private Expression parseExponentiation() {
		SourceLocation location = currentToken.sourceLocation;
		// Kodierung der Logik
		Expression expoErgeb = parseDotProd();
		while (currentToken.type == EXP){
			acceptIt();
			expoErgeb = new Exponentiation(location, expoErgeb, parseDotProd());
		}
		return expoErgeb;
	}
	
	private Expression parseDotProd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseMatrixMul();
		while(currentToken.type == DOTPROD) {
			acceptIt();
			x = new DotProduct(location, x, parseMatrixMul());
		}
		return x;
	}
	
	private Expression parseMatrixMul() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseTranspose();
		while(currentToken.type == MATMULT) {
			acceptIt();
			x = new MatrixMultiplication(location, x, parseTranspose());
		}
		return x;
	}
	
	private Expression parseTranspose() {
		// Anwendung von der Methode dim in der grammatik: Dann ist der zweite Parameter die Methode parseDim()
		SourceLocation location = currentToken.sourceLocation;
		if(currentToken.type == TRANSPOSE) {
			acceptIt();
			 return new MatrixTranspose(location, parseDim());
		}
		return parseDim(); // liefert einfach den Aufruf von der entsprechenden Methode zurück
	}
	
	private Expression parseDim() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseSubRange();
		switch(currentToken.type) {
			case ROWS:
				acceptIt();
				return new MatrixRows(location, x);
			case COLS:
				acceptIt();
				return new MatrixCols(location, x);
			case DIM:
				acceptIt();
				return new VectorDimension(location, x);
			default:
				return x;
		}
	}
	
	private Expression parseSubRange() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseElementSelect();
		
		if(currentToken.type == LBRACE) {
			acceptIt();
			Expression xStartIndex = parseExpr();
			accept(COLON);
			Expression xBaseIndex = parseExpr();
			accept(COLON);
			Expression xEndIndex = parseExpr();
			accept(RBRACE);
			if(currentToken.type != LBRACE)
				return new SubVector(location, x, xBaseIndex, xStartIndex, xEndIndex);
			
			accept(LBRACE);
			Expression yStartIndex = parseExpr();
			accept(COLON);
			Expression yBaseIndex = parseExpr();
			accept(COLON);
			Expression yEndIndex = parseExpr();
			accept(RBRACE);
			return new SubMatrix(location, x, xBaseIndex, xStartIndex, xEndIndex, yBaseIndex, yStartIndex, yEndIndex);
		}
		
		return x;
	}
	
	private Expression parseElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		Expression result = parseRecordElementSelect();
		while (currentToken.type == LBRACKET){
			acceptIt();
			Expression value = parseExpr();
			accept(RBRACKET);
			result= new ElementSelect(location, result, value);
		}
		return result;
	}
	
	private Expression parseRecordElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAtom();
		
		if(currentToken.type == AT) {
			accept(AT);
			String elementName = accept(ID);
			x = new RecordElementSelect(location, x, elementName);
		}
		
		return x;
	}
	
	private Expression parseAtom() {
		SourceLocation location = currentToken.sourceLocation;
		
		switch(currentToken.type) {
			case INTLIT:
				return new IntValue(location, parseIntLit());
			case FLOATLIT:
				return new FloatValue(location, parseFloatLit());
			case BOOLLIT:
				return new BoolValue(location, parseBoolLit());
			case STRINGLIT:
				return new StringValue(location, accept(STRINGLIT));
			default: /* check other cases below */
		}
		
		if(currentToken.type == ID) {
			String name = accept(ID);
			if(currentToken.type != LPAREN) {
				return new IdentifierReference(location, name);
				
			} else {
				return parseCall(name, location);
			}
		}
		
		if(currentToken.type == LPAREN) {
			acceptIt();
			Expression x = parseExpr();
			accept(RPAREN);
			return x;
		}
		
		if(currentToken.type == AT) {
			acceptIt();
			String name = accept(ID);
			return new RecordInit(location, name, parseInitializerList());
		}
		
		if(currentToken.type == LBRACKET) {
			return new StructureInit(location, parseInitializerList());
		}
		
		throw new SyntaxError(currentToken, INTLIT, FLOATLIT, BOOLLIT, STRINGLIT, ID, LPAREN, LBRACKET, AT);
	}
	
	private List<Expression> parseInitializerList() {
		List<Expression> elements = new ArrayList<>();
		
		accept(LBRACKET);
		elements.add(parseExpr());
		while(currentToken.type == COMMA) {
			accept(COMMA);
			elements.add(parseExpr());
		}
		accept(RBRACKET);
		
		return elements;
	}
	
	private int parseIntLit() {
		return Integer.parseInt(accept(INTLIT));
	}
	
	private float parseFloatLit() {
		return Float.parseFloat(accept(FLOATLIT));
	}
	
	private boolean parseBoolLit() {
		return Boolean.parseBoolean(accept(BOOLLIT));
	}
}
