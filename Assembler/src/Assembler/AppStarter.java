package Assembler;

import java.awt.EventQueue;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;
import javax.swing.border.BevelBorder;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.JTextArea;
import java.awt.Color;

public class AppStarter {

	private static JFrame frame = new JFrame();
	private static JMenuBar menuBar = new JMenuBar();
	private JTextArea textArea = new JTextArea();
	private Pass2 pass2;
	private Load load;
	private Handle handle;
	private ArrayList<String> lines = new ArrayList<>();
	private ArrayList<String> recordFile = new ArrayList<>();
	private ArrayList<String> listingFile = new ArrayList<>();
	private ArrayList<String> intemediateFile = new ArrayList<>();

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			private AppStarter window;

			public void run() {
				try {
					window = new AppStarter();
					frame.setSize(1366, 740);
					frame.setJMenuBar(menuBar);
					frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public AppStarter() {
		handle = new Handle();
		load = new Load();
		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void loadFiles() {
		lines = load.loadProgram();
	}

	private void initialize() {
		menuBar = new JMenuBar();
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().setLayout(null);
		textArea = new JTextArea();
		textArea.setForeground(Color.BLACK);
		textArea.setBackground(Color.WHITE);
		textArea.setFont(new Font("Consolas", Font.PLAIN /* | Font.ITALIC */, 18));
		textArea.setEditable(true);
		JScrollPane scroll = new JScrollPane(textArea);
		scroll.setBounds(10, 11, 1330, 658);
		scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		scroll.setViewportBorder(new BevelBorder(BevelBorder.LOWERED, null, null, null, null));
		frame.getContentPane().add(scroll);
		menuBar = new JMenuBar();
		JMenu file = new JMenu("File");
		file.setFont(new Font("Segoe UI", Font.BOLD, 14));
		menuBar.add(file);
		JMenu assemble = new JMenu("Assembling");
		assemble.setFont(new Font("Segoe UI", Font.BOLD, 14));
		menuBar.add(assemble);
		JMenuItem nNew = new JMenuItem("New");
		nNew.setFont(new Font("Segoe UI", Font.PLAIN, 14));
		nNew.setHorizontalAlignment(SwingConstants.LEFT);
		nNew.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent arg0) {
				textArea.setText(null);
			}
		});
		file.add(nNew);

		JMenuItem open = new JMenuItem("Open");
		open.setFont(new Font("Segoe UI", Font.PLAIN, 14));
		open.setHorizontalAlignment(SwingConstants.LEFT);
		open.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				textArea.setText(null);
				loadFiles();
				for (int i = 0; i < lines.size(); i++) {
					textArea.append(lines.get(i) + "\n");
				}
				textArea.append("\n");
			}
		});
		file.add(open);

		JMenuItem save = new JMenuItem("Save as");
		save.setFont(new Font("Segoe UI", Font.PLAIN, 14));
		save.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				saveAs();
			}
		});
		file.add(save);

		JMenuItem exit = new JMenuItem("Exit");
		exit.setFont(new Font("Segoe UI", Font.PLAIN, 14));
		exit.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				System.exit(0);
			}
		});
		file.add(exit);

		JMenuItem assembling = new JMenuItem("Assemble");
		assembling.setFont(new Font("Segoe UI", Font.PLAIN, 14));
		assembling.setHorizontalAlignment(SwingConstants.LEFT);
		assembling.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				String n = textArea.getText();
				String[] Lines = n.split("\n");
				for (int i = 0; i < Lines.length; i++) {
					lines.add(Lines[i]);
				}
				pass2 = new Pass2(lines);
				lines = pass2.getUnHandleLines();
				lines = handle.convertToUpper(lines);

			}
		});
		assemble.add(assembling);

		JMenuItem interFile = new JMenuItem("Intermediate File");
		interFile.setFont(new Font("Segoe UI", Font.PLAIN, 14));
		interFile.setHorizontalAlignment(SwingConstants.LEFT);
		interFile.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				textArea.setText(null);
				intemediateFile = pass2.getIntermediateFile();
				intemediateFile = handle.convertToUpper(intemediateFile);
				for (int i = 0; i < intemediateFile.size(); i++) {
					textArea.append(intemediateFile.get(i) + "\n");
				}
				textArea.append("\n");
			}
		});
		assemble.add(interFile);

		JMenuItem ListFile = new JMenuItem("Listing File");
		ListFile.setFont(new Font("Segoe UI", Font.PLAIN, 14));
		ListFile.setHorizontalAlignment(SwingConstants.LEFT);
		ListFile.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				textArea.setText(null);
				listingFile = pass2.getListFile();
				for (int i = 0; i < listingFile.size(); i++) {
					textArea.append(listingFile.get(i) + "\n");
				}
				textArea.append("\n");
			}
		});
		assemble.add(ListFile);

		JMenuItem recFile = new JMenuItem("Object File");
		recFile.setFont(new Font("Segoe UI", Font.PLAIN, 14));
		recFile.setHorizontalAlignment(SwingConstants.LEFT);
		recFile.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent arg0) {
				textArea.setText(null);
				recordFile = pass2.getRecords();
				for (int i = 0; i < recordFile.size(); i++) {
					textArea.append(recordFile.get(i) + "\n");
				}
				textArea.append("\n");
			}
		});
		assemble.add(recFile);
	}

	private void saveAs() {
		load = new Load();
		File file = load.chooseFileSave();
		String url = null;
		BufferedWriter outputWriter = null;
		if (file != null) {
			url = file.getAbsolutePath();
			while (!url.substring(url.length() - 4).equals(".txt")) {
				System.out.println("Error! choose only \".txt\" files");
				url = load.chooseFn().getAbsolutePath();
			}
			try {
				outputWriter = new BufferedWriter(new FileWriter(url));
				String n = textArea.getText();
				String[] Lines = n.split("\n");
				for (int i = 0; i < Lines.length; i++) {
					outputWriter.write(Lines[i]);
					outputWriter.newLine();
				}
				outputWriter.flush();
				outputWriter.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}
