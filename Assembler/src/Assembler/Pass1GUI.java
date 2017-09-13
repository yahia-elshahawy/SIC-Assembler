package Assembler;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.LinkedList;

public class Pass1GUI {

	private Hashtable<String, String> opTab;
	private Hashtable<String, String> symTab;
	private LinkedList<String> locTab;
	private ArrayList<String> lines;
	private ArrayList<String> linesHandled;
	private ArrayList<String> literals;
	private Hashtable<String, String> litTab;
	private boolean lenghtFlag;
	private ArrayList<String> intermediateFile;
	private int maxLength = 65535;
	private int currentLoc;
	private Handle handle = new Handle();

	public Hashtable<String, String> getLitTab() {
		return litTab;
	}

	public Hashtable<String, String> getOpTab() {
		return opTab;
	}

	public Hashtable<String, String> getSymTab() {
		return symTab;
	}

	public LinkedList<String> getLocTab() {
		return locTab;
	}

	public ArrayList<String> getUnHandleLines() {
		return lines;
	}
	
	public ArrayList<String> getLines() {
		return linesHandled;
	}

	public int getBytes(String operand) {
		String bytes = operand.substring(2, operand.length() - 1);
		return bytes.length();
	}

	public int getBytesLiterals(String literal) {
		String bytes = literal.substring(3, literal.length() - 1);
		return bytes.length();
	}

	public String getLabel(String line) {
		return line.substring(0, 8).trim();
	}

	public String getOpCode(String line) {
		if (line.substring(9, 11).equals("=x") || line.substring(9, 11).equals("=c"))
			return line.substring(9, line.length()).trim();
		if (line.length() < 15)
			return line.substring(9, line.length()).trim();
		return line.substring(9, 15).trim();
	}

	public String getOperand(String line) {
		if (line.length() < 17)
			return "";
		if (line.length() < 36)
			return line.substring(17).trim();
		else
			return line.substring(17, 35).trim();
	}

	public Pass1GUI() {
		opTab = new Hashtable<>();
		symTab = new Hashtable<>();
		litTab = new Hashtable<>();
		locTab = new LinkedList<>();
		lines = new ArrayList<>();
		linesHandled = new ArrayList<>();
		literals = new ArrayList<>();
		currentLoc = 0;
		lenghtFlag = false;
		intermediateFile = new ArrayList<>();
	}

	public ArrayList<String> getIntermediateFile() {
		return intermediateFile;
	}

	public void Start(ArrayList<String>LINES) {
		lines=LINES;
		loadFiles();
		checkLinesFormat();
		lowerCase();
		translateFile();
		IntermediateFile();
		writeIntermediateFile();
//		printPass1();
	}

	private void loadFiles() {
		Load loadObj = new Load();
//		lines = loadObj.loadProgram();
		opTab = loadObj.loadOpCode();
	}

	private void checkLinesFormat() {
		LineChecker checker = new LineChecker();
		String line = "";
		for (int i = 0; i < lines.size(); i++) {
			line = lines.get(i);
			if (line.charAt(0) == '.') {
				lines.remove(i);
				i--;
				continue;
			}
			if (!checker.isCorrect(line)) {
				System.out.println("Error in code format!");
				System.exit(0);
			}
		}
	}

	private boolean handleEquate(String label, String operand, int currentLoction) {
		String address = "";
		if (operand.contains("+") || operand.contains("-"))
			return handleEquate2(label, operand, address);
		else if (symTab.containsKey(operand))
			return handleEquate3(label, operand, address);
		else if (operand.equals("*"))
			return handleEquate4(label, operand, address, currentLoction);
		return false;
	}

	private boolean handleEquate2(String label, String operand, String address) {
		int ind = operand.indexOf("+");
		if (ind == -1)
			ind = operand.indexOf("-");
		String operand1 = operand.substring(0, ind);
		operand1 = operand1.trim();
		String operand2 = operand.substring(ind + 1, operand.length());
		operand2 = operand2.trim();
		if (symTab.containsKey(operand1)) {
			int num1 = Integer.parseInt(symTab.get(operand1), 16);
			int num2;
			if (symTab.containsKey(operand2))
				num2 = Integer.parseInt(symTab.get(operand2), 16);
			else
				num2 = Integer.parseInt(operand2, 16);
			if (operand.charAt(ind) == '+')
				address = Integer.toHexString(num1 + num2);
			else
				address = Integer.toHexString(num1 - num2);
			symTab.put(label, address);
			locTab.add(address);
			return true;
		}
		return false;
	}

	private boolean handleEquate3(String label, String operand, String address) {
		address = symTab.get(operand);
		symTab.put(label, address);
		locTab.add(address);
		return true;
	}

	private boolean handleEquate4(String label, String operand, String address, int currentLoction) {
		address = Integer.toHexString(currentLoction);
		symTab.put(label, address);
		locTab.add(address);
		return true;
	}

	private void handleLiteral() {
		if (literals.size() == 0)
			return;
		int lastLocation = currentLoc;
		for (int i = 0; i < literals.size(); i++) {
			locTab.add(Integer.toHexString(lastLocation));

			String literal = literals.get(i);
			int counter = getBytesLiterals(literal);
			if (literal.substring(0, 2).equals("=c")) {
				lastLocation += counter;
			} else {
				if (getBytes(literal) % 2 == 0) {
					counter /= 2;
				} else {
					counter /= 2;
					counter++;
				}
				lastLocation += counter;
			}
			if (!litTab.containsKey(literal)) {
				litTab.put(literal, "" + Integer.toHexString(currentLoc));
				linesHandled.add("*        " + literals.get(i));
				currentLoc = lastLocation;
			}
		}
		literals.clear();
		locTab.add(Integer.toHexString(currentLoc));
	}

	private void lowerCase() {
		for (int i = 0; i < lines.size(); i++)
			lines.set(i, lines.get(i).toLowerCase());
	}

	private void translateFile() {

		int index = 0;
		String line = lines.get(index);
		String label = getLabel(line);
		String opCode = getOpCode(line);
		String operand = getOperand(line);
		int orgLocation = 0;
		boolean orgFlag = false;

		if (opCode.equals("start")) {
			linesHandled.add(line);
			currentLoc = Integer.parseInt(operand, 16);
			index++;
			locTab.add(operand);
		}
		locTab.add(operand);
		while (!opCode.equals("end")) {
			line = lines.get(index);
			linesHandled.add(line);
			label = getLabel(line);
			opCode = getOpCode(line);
			operand = getOperand(line);
			if (symTab.containsKey(label) && !label.equals("*")) {
				System.out.println("Error in labels , there is duplicates! : " + label);
				System.exit(0);
			}
			if (!label.isEmpty()) {
				if (!opCode.equals("equ"))
					symTab.put(label, "" + Integer.toHexString(currentLoc));
			}
			if (opTab.containsKey(opCode)) {
				if (operand.length() > 2 && (operand.substring(0, 2).equals("=x")
						|| operand.substring(0, 2).equals("=c") || operand.substring(0, 2).equals("=w")))
					literals.add(operand);
				else if (operand.length() > 2 && operand.substring(0, 2).equals("0x"))
					symTab.put(operand, "" + Integer.toHexString(currentLoc));
				currentLoc += 3;
			} else if (opCode.equals("equ")) {
				boolean error = handleEquate(label, operand, currentLoc);
				if (!error) {
					System.out.println("Equate error in operand in line: " + index);
					break;
				}
			} else if (opCode.equals("ltorg")) {
				handleLiteral();
				index++;
				continue;
			} else if (opCode.equals("org")) {
				if (!orgFlag) {
					orgLocation = currentLoc;
					if (symTab.containsKey(operand)) {
						currentLoc = Integer.parseInt(symTab.get(operand), 16);
						orgFlag = true;
					} else {
						System.out.println("ORG error in operand in line: " + index);
						break;
					}
				} else {
					currentLoc = orgLocation;
					orgFlag = false;
				}
			} else if (opCode.equals("word"))
				currentLoc += 3;
			else if (opCode.equals("resw"))
				currentLoc += (3 * Integer.parseInt(operand));
			else if (opCode.equals("resb"))
				currentLoc += Integer.parseInt(operand);
			else if (opCode.equals("byte"))
				byteHandle(operand);
			else if (opCode.equals("end"))
				continue;
			else {
				System.out.println("Error in label in line: " + index);
				System.exit(0);
			}
			if (index + 1 < lines.size() && getOpCode(lines.get(index + 1)).equals("equ")) {
				index++;
				continue;
			}
			if (currentLoc >= maxLength) {
				lenghtFlag = true;
				System.out.println("Error in the programm size ");
				System.exit(0);
			}
			locTab.add(Integer.toHexString(currentLoc));
			index++;
		}
		handleLiteral();
	}

	private void byteHandle(String operand) {
		if (operand.charAt(0) == 'c')
			currentLoc += getBytes(operand);
		else {
			int counter = getBytes(operand);
			if (getBytes(operand) % 2 == 0) {
				counter /= 2;
			} else {
				counter /= 2;
				counter++;
			}
			currentLoc += counter;
		}
	}

	private void IntermediateFile() {
		if (lenghtFlag)
			return;
		for (int i = 0; i < linesHandled.size(); i++) {
			String listLine = "";
			String line = linesHandled.get(i);
			listLine = listLine + handle.chechLength(locTab.get(i), 6) + "   " + line;
			intermediateFile.add(listLine);
		}
	}

	public void writeIntermediateFile() {
		String url = "D:\\workshop\\Intermediate.txt";
		BufferedWriter outputWriter = null;
		try {
			intermediateFile=handle.convertToUpper(intermediateFile);
			outputWriter = new BufferedWriter(new FileWriter(url));
			for (int i = 0; i < intermediateFile.size(); i++) {
				outputWriter.write(intermediateFile.get(i));
				outputWriter.newLine();

			}
			outputWriter.flush();
			outputWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void printPass1() {
		if (lenghtFlag)
			return;
		String leftAlignFormat = "| %-15s | %-35s |%n";
		System.out.format("+-----------------+-------------------------------------+%n");
		System.out.format("|    Location     | .2345678901234567890123456789012345 |%n");
		System.out.format("+-----------------+-------------------------------------+%n");
		for (int i = 0; i < linesHandled.size(); i++)
			System.out.format(leftAlignFormat, locTab.get(i), linesHandled.get(i));
		System.out.format("+-----------------+---------------f----------------------+%n");

		System.out.println();
		Enumeration<String> keys = symTab.keys();
		System.out.format("+------------+--------+%n");
		System.out.format("| Location   | Symbol |%n");
		System.out.format("+------------+--------+%n");
		while (keys.hasMoreElements()) {
			String key = keys.nextElement();
			System.out.format("| %-10s | %-5s |%n", symTab.get(key), key);
		}
		System.out.format("+------------+--------+%n");

		Enumeration<String> keyss = litTab.keys();
		System.out.println();
		System.out.format("+------------+--------+%n");
		System.out.format("| Location   | literals |%n");
		System.out.format("+------------+--------+%n");
		while (keys.hasMoreElements()) {
			String key = keyss.nextElement();
			System.out.format("| %-10s | %-5s |%n", litTab.get(key), key);
		}
		System.out.format("+------------+--------+%n");
	}
}
