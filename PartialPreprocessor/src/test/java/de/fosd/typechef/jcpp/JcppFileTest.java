package de.fosd.typechef.jcpp;

import java.io.IOException;

import org.anarres.cpp.LexerException;
import org.junit.Ignore;
import org.junit.Test;

/**
 * test output with .check files
 * 
 * @author kaestner
 * 
 */
public class JcppFileTest extends AbstractCheckTests {

	@Test
	public void testNestingDead() throws LexerException, IOException {
		testFile("nestingdead.c");
	}

	@Test
	public void testDeadElse() throws LexerException, IOException {
		testFile("deadelse.h");
	}

	@Test
	public void testIncludeGuard() throws LexerException, IOException {
		testFile("in1.c");
	}

	@Test
	public void testUnlikely() throws LexerException, IOException {
		testFile("unlikely.h");
	}

	@Test
	public void testByteOrder() throws LexerException, IOException {
		testFile("byteorder.h");
	}

	@Test
	@Ignore
	public void testIf() throws LexerException, IOException {
		testFile("if.h");
	}

	@Test
	public void testAlternativeMacros() throws LexerException, IOException {
		testFile("macro2.c");
	}

	@Test
	public void testIncludeGuards() throws LexerException, IOException {
		testFile("includeguards.c");
	}

	@Test
	public void testIncludeGuards2() throws LexerException, IOException {
		testFile("includeguards2.h");
	}

	@Test
	public void testDefDefined() throws LexerException, IOException {
		testFile("defdefined.c");
	}

	@Test
	public void testAlternativeDef() throws LexerException, IOException {
		testFile("alternativedef.c");
	}

	@Test
	public void testHiddenBaseAndDead() throws LexerException, IOException {
		testFile("hiddenDeadAndBase.c");
	}

	@Test
	@Ignore
	public void testMultiInclude() throws LexerException, IOException {
		// XXX this is not supported right now. let's see whether we will need
		// it.
		testFile("multiinclude.c");
	}

	@Test
	public void testIfElseParsing() throws LexerException, IOException {
		testFile("ifcondition.c");
	}

	@Test
	public void testBeispielJoerg() throws LexerException, IOException {
		testFile("beispielJoerg.c");
	}

	@Test
	public void testNumericIfAlternative() throws LexerException, IOException {
		testFile("ifdefnumeric.c");
	}

	@Test
	public void testLinuxTestFLock() throws LexerException, IOException {
		testFile("linuxtestflock.c");
	}

	@Test
	public void testElIfChain() throws LexerException, IOException {
		testFile("elifchain.c");
	}

	@Test
	public void testSelfDef() throws LexerException, IOException {
		testFile("selfdef.c");
	}

	@Test
	public void testNonTautologicExpansions() throws LexerException,
			IOException {
		testFile("non_tautologic.c");
	}

	@Test
	public void testVariadic() throws LexerException, IOException {
		testFile("variadic.c");
	}

	@Test
	public void testIncompMacroExp() throws LexerException, IOException {
		testFile("incompatibleMacroExp.c");
	}

	@Test
	public void testRedef() throws LexerException, IOException {
		testFile("redef.h");
	}
	
	//jiffies contains complex calculations; from the linux kernel headers
	@Test
	public void testJiffies() throws LexerException, IOException {
		testFile("jiffiesTest.h");
	}
	
	@Test
	public void testIncludeMacros() throws LexerException, IOException {
		testFile("includemacro.c");
	}
	
}
