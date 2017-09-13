package Assembler;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.Stack;

public class Pass1 {

	private Hashtable<String, String> opTab;
	private Hashtable<String, String> symTab;
	private LinkedList<String> locTab;
	private ArrayList<String> lines;
	private ArrayList<String> linesHandled;
	private ArrayList<String> literals;
	private ArrayList<Integer> duplicatedArray;
	private ArrayList<Integer> outOfRange;
	private ArrayList<Integer> outOfLength;
	private ArrayList<Integer> undefinedSymble;
	private Hashtable<String, String> litTab;
	private ArrayList<String> intermediateFile;
	private Stack<Integer> orgLocation = new Stack<>();

	private int maxLength = 65535;
	private int currentLoc;
	private Handle handle = new Handle();

	public Pass1() {
		opTab = new Hashtable<>();
		symTab = new Hashtable<>();
		litTab = new Hashtable<>();
		locTab = new LinkedList<>();
		lines = new ArrayList<>();
		linesHandled = new ArrayList<>();
		literals = new ArrayList<>();
		duplicatedArray = new ArrayList<>();
		outOfRange = new ArrayList<>();
		outOfLength = new ArrayList<>();
		undefinedSymble = new ArrayList<>();
		currentLoc = 0;
		intermediateFile = new ArrayList<>();
	}

	public void Start(ArrayList<String> LINES) {
		lines = LINES;
		loadFiles();
		checkLinesFormat();
		lowerCase();
		translateFile();
		checkOperands();
		catchError();
		IntermediateFile();
		writeIntermediateFile();
		// printPass1();
	}

	// *********CHECK FORMATE FUNCTIONS**********//

	private void loadFiles() {
		Load loadObj = new Load();
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

	private void handleORG(String label, String operand, int currentLoction) {
		String address = "";
		if (operand.contains("+") || operand.contains("-")) {
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
				orgLocation.push(currentLoc);
				currentLoc = Integer.parseInt(address, 16);
			}
		}

	}

	// ************BUILD FUNCTION*************//

	private void translateFile() {
		int index = 0;
		String line = lines.get(index);
		String label = getLabel(line);
		String opCode = getOpCode(line);
		String operand = getOperand(line);
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
				duplicatedArray.add(index);
			}
			if (!label.isEmpty()) {
				if (!opCode.equals("equ"))
					symTab.put(label, "" + Integer.toHexString(currentLoc));
			}
			if (opTab.containsKey(opCode)) {
				if (operand.length() > 2 && operand.substring(0, 1).equals("="))
					literals.add(operand);
				else if (operand.length() > 2 && operand.substring(0, 2).equals("0x"))
					symTab.put(operand, "" + Integer.toHexString(currentLoc));
				currentLoc += 3;
			} else if (opCode.equals("equ")) {
				boolean error = handleEquate(label, operand, currentLoc);
				if (!error) {
					undefinedSymble.add(index);
					System.out.println("Equate error in operand in line: " + index);
				}
			} else if (opCode.equals("ltorg")) {
				handleLiteral();
				index++;
				continue;
			} else if (opCode.equals("org")) {
				if (!operand.equals("")) {
					if (operand.contains("+") || operand.contains("-")) {
						handleORG(label, operand, currentLoc);
					} else if (symTab.containsKey(operand)) {
						orgLocation.push(currentLoc);
						currentLoc = Integer.parseInt(symTab.get(operand), 16);
					} else {
						try {
							operand = handle.chechLength(Integer.toHexString(Integer.parseInt(operand, 16)),
									operand.length());
							orgLocation.push(currentLoc);
							currentLoc = Integer.parseInt(operand, 16);
						} catch (Exception e) {
							undefinedSymble.add(index);
							currentLoc = 0;
							System.out.println("ORG error in operand in line: " + index);
						}
					}
				} else {
					if (orgLocation.size() > 0)
						currentLoc = orgLocation.pop();
					else {
						undefinedSymble.add(index);
						currentLoc = 0;
						System.out.println("ORG error in operand in line: " + index);
					}
				}
			} else if (opCode.equals("word"))
				currentLoc += 3;
			else if (opCode.equals("resw"))
				currentLoc += (3 * Integer.parseInt(operand));
			else if (opCode.equals("resb"))
				currentLoc += Integer.parseInt(operand);
			else if (opCode.equals("byte"))
				handleByte(operand, index);
			else if (opCode.equals("end"))
				continue;
			else {
				System.out.println("Error in label in line: " + index);
				undefinedSymble.add(index);
			}
			if (index + 1 < lines.size() && getOpCode(lines.get(index + 1)).equals("equ")) {
				index++;
				continue;
			}
			if (currentLoc >= maxLength) {
				System.out.println("Error in the programm size ");
				outOfLength.add(index);
			}
			locTab.add(Integer.toHexString(currentLoc));
			index++;
		}
		handleLiteral();
	}

	// *********HANDLE FUNCTIONS**************//

	private void handleByte(String operand, int index) {
		if (operand.charAt(0) == 'c') {
			String check = handle.toHex(operand.substring(2, operand.length() - 1));
			// if (check.length() > 6)
			// outOfRange.add(index);
			currentLoc += getBytes(operand);
		} else {
			int counter = getBytes(operand);
			if (getBytes(operand) % 2 == 0) {
				counter /= 2;

			} else {
				counter /= 2;
				counter++;
			}
			// if (operand.substring(2, operand.length() - 1).length() > 6)
			outOfRange.add(index);
			currentLoc += counter;
		}
	}

	private boolean handleEquate(String label, String operand, int currentLoction) {
		String address = "";
		if (operand.contains("+") || operand.contains("-"))
			return handleEquate1(label, operand, address);
		else if (symTab.containsKey(operand))
			return handleEquate2(label, operand, address);
		else if (operand.equals("*"))
			return handleEquate3(label, operand, address, currentLoction);
		return handleEquate4(label, operand, currentLoction);
	}

	private boolean handleEquate1(String label, String operand, String address) {
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

	private boolean handleEquate2(String label, String operand, String address) {
		address = symTab.get(operand);
		symTab.put(label, address);
		locTab.add(address);
		return true;
	}

	private boolean handleEquate3(String label, String operand, String address, int currentLoction) {
		address = Integer.toHexString(currentLoction);
		symTab.put(label, address);
		locTab.add(address);
		return true;
	}

	private boolean handleEquate4(String label, String operand, int currentLoction) {
		char start;
		boolean ch = false;
		for (int i = 0; i < operand.length(); i++) {
			start = operand.charAt(0);
			if (!(Character.isDigit(start))) {
				ch = true;
				break;
			}
		}
		if (!ch) {
			String add = Integer.toHexString(Integer.parseInt(operand));
			symTab.put(label, add);
			locTab.add(add);
			return true;
		}
		return false;
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

	private void checkOperands() {
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			String opCode = getOpCode(line);
			String operand = getOperand(line);
			if (opCode.equals("org") || opCode.equals("ltorg") || opCode.equals("resw") || opCode.equals("resb")
					|| opCode.equals("word") || opCode.equals("byte") || opCode.equals("start") || opCode.equals("rsub")
					|| opCode.equals("equ") || opCode.equals("end")) {
				continue;
			}
			if (operand.length() > 2 && (operand.substring(0, 2).equals("=x") || operand.substring(0, 2).equals("=c")
					|| operand.substring(0, 2).equals("=w") || operand.substring(0, 2).equals("0x")))
				continue;
			if (operand.length() > 2 && operand.substring(operand.length() - 2).equals(",x")) {
				if (!symTab.containsKey(operand.substring(0, operand.length() - 2)))
					undefinedSymble.add(i);
				continue;
			}
			if (operand.equals("") || operand.equals("*"))
				continue;
			if (!symTab.containsKey(operand)) {
				undefinedSymble.add(i);
			}
		}
	}

	private void lowerCase() {
		for (int i = 0; i < lines.size(); i++)
			lines.set(i, lines.get(i).toLowerCase());
	}

	public boolean catchError() {
		if (getDuplicatedArray().size() > 0 || getOutOfRange().size() > 0 || getUndefinedSymble().size() > 0
				|| getOutOfLength().size() > 0)
			return true;
		return false;
	}

	private void IntermediateFile() {
		for (int i = 0; i < linesHandled.size(); i++) {
			String listLine = "";
			String line = linesHandled.get(i);
			listLine = listLine + handle.chechLength(locTab.get(i), 6) + "   " + line;
			if (duplicatedArray.size() > 0) {
				for (int j = 0; j < duplicatedArray.size(); j++) {
					if (i == duplicatedArray.get(j)) {
						listLine = listLine + "    Error in labels , there is duplicates! : ";
						break;
					}
				}
			}
			if (outOfRange.size() > 0) {
				for (int j = 0; j < outOfRange.size(); j++) {
					if (i == outOfRange.get(j)) {
						listLine = listLine + "    Error out of range in this line : ";
						break;
					}
				}
			}
			if (outOfLength.size() > 0) {
				for (int j = 0; j < outOfLength.size(); j++) {
					if (i == outOfLength.get(j)) {
						listLine = listLine + "    Error in the length of the program  ";
						break;
					}
				}
			}
			if (undefinedSymble.size() > 0) {
				for (int j = 0; j < undefinedSymble.size(); j++) {
					if (i == undefinedSymble.get(j)) {
						listLine = listLine + "    Error Undefined symble ";
						break;
					}
				}
			}
			intermediateFile.add(listLine);
		}
	}

	private void writeIntermediateFile() {
		String url = "D:\\workshop\\Intermediate.txt";
		BufferedWriter outputWriter = null;
		try {
			intermediateFile = handle.convertToUpper(intermediateFile);
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

	// **************GETTER FUNCTIONS ************//

	public Hashtable<String, String> getLitTab() {
		return litTab;
	}

	public Hashtable<String, String> getOpTab() {
		return opTab;
	}

	public ArrayList<Integer> getDuplicatedArray() {
		return duplicatedArray;
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

	public ArrayList<Integer> getUndefinedSymble() {
		return undefinedSymble;
	}

	public ArrayList<Integer> getOutOfRange() {
		return outOfRange;
	}

	public ArrayList<Integer> getOutOfLength() {
		return outOfLength;
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

	public ArrayList<String> getIntermediateFile() {
		return intermediateFile;
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

	//////////////////////////////////////////////////////////////////////////////////

	private void printPass1() {
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
