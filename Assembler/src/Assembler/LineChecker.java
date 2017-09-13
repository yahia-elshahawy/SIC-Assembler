package Assembler;

public class LineChecker {

	public boolean isCorrect(String line) {
		String label = line.substring(0, 8).trim();
		String opCode = line.substring(9, line.length()).trim().toLowerCase();
		if (line.length() < 18) {
			if (!label.isEmpty() && (hasSpaces(label) || startInvalid(label)))
				return false;
			if (opCode.equals("ltorg") || opCode.equals("org") || opCode.equals("rsub"))
				return true;
		}
		if (line.length() > 66 || line.length() < 18)
			return false;
		if (line.charAt(8) != ' ' || line.charAt(15) != ' ' || line.charAt(16) != ' ')
			return false;
		opCode = line.substring(9, 15).trim();
		String operand = "";
		if (line.length() < 36)
			operand = line.substring(17).trim();
		else
			operand = line.substring(17, 35).trim();
		if (!label.isEmpty() && (hasSpaces(label) || startInvalid(label)))
			return false;
		if (startInvalid(opCode))
			return false;
		if (hasSpaces(operand) && operand.charAt(0) != 'c')
			return false;
		return true;
	}

	public boolean startInvalid(String word) {
		char start = word.charAt(0);
		if (!(Character.isDigit(start) || Character.isAlphabetic(start)))
			return true;
		return false;
	}

	private boolean hasSpaces(String word) {
		for (int i = 0; i < word.length(); i++) {
			if (word.charAt(i) == ' ')
				return true;
		}
		return false;
	}
}
