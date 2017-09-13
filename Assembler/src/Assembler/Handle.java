package Assembler;

import java.math.BigInteger;
import java.util.ArrayList;


public class Handle {

	public String chechLength(String objectCode, int Limit) {
		while (objectCode.length() < Limit) {
			objectCode = "0" + objectCode;
		}
		if (objectCode.length() > Limit) {
			objectCode = objectCode.substring(0, Limit);
		}
		return objectCode;
	}

	public String rehandleLable(String label) {
		while (label.length() < 6) {
			label = label + " ";
		}
		return label;
	}

	public String toHex(String arg) {
		arg = arg.toUpperCase();
		arg = String.format("%040x", new BigInteger(1, arg.getBytes(/* YOUR_CHARSET? */)));
		char firstChar = arg.charAt(0);
		while (firstChar == '0') {
			arg = arg.substring(1, arg.length());
			firstChar = arg.charAt(0);
		}
		return arg;
	}

	public ArrayList<String> convertToUpper(ArrayList<String> lines) {
		ArrayList<String> newLines = new ArrayList<>();
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			line = line.toUpperCase();
			newLines.add(line);
		}
		return newLines;
	}
}
