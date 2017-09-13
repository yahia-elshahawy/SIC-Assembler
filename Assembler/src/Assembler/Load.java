package Assembler;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

public class Load {
	private BufferedReader saveFile;
	private JFileChooser chooser;

	public Load() {
		chooser = new JFileChooser();
	}

	public File chooseFileSave() {
		FileNameExtensionFilter txtFilter = new FileNameExtensionFilter("tesxt files (*.txt)", "txt");
		chooser.addChoosableFileFilter(txtFilter);
		chooser.setFileFilter(txtFilter);
		chooser.setSelectedFile(new File("Untitled.txt"));
		chooser.showSaveDialog(null);
		File file = chooser.getSelectedFile();
		return file;
	}

	public File chooseFn() {
		FileNameExtensionFilter txtFilter = new FileNameExtensionFilter("tesxt files (*.txt)", "txt");
		chooser.addChoosableFileFilter(txtFilter);
		chooser.setFileFilter(txtFilter);
		chooser.showOpenDialog(null);
		File file = chooser.getSelectedFile();
		return file;
	}

	public ArrayList<String> loadProgram() {
		ArrayList<String> words = new ArrayList<>();
		File file = chooseFn();
		String url = null;
		if (file != null) {
			url = file.getAbsolutePath();

			while (!url.substring(url.length() - 4).equals(".txt")) {
				System.out.println("Error! choose only \".txt\" files");
				url = chooseFn().getAbsolutePath();
			}
			try {
				saveFile = new BufferedReader(new FileReader(url));
				String line = saveFile.readLine();
				while (line != null) {
					words.add(line);
					line = saveFile.readLine();
				}
				saveFile.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return words;
	}

	public Hashtable<String, String> loadOpCode() {
		ArrayList<String> appendix = new ArrayList<String>();
		File file = new File("src/Materials/Appendix.txt");
		String url = null;
		Hashtable<String, String> opTab = new Hashtable<>();
		if (file != null) {
			url = file.getAbsolutePath();
			try {
				saveFile = new BufferedReader(new FileReader(url));
				for (int i = 0; i < 25; i++)
					appendix.add(saveFile.readLine());
				saveFile.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
			for (int i = 0; i < appendix.size(); i++) {
				String[] opcode = appendix.get(i).split(" ");
				opTab.put(opcode[0], opcode[1]);
			}
		}
		return opTab;
	}

}
