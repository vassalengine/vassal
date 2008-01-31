/*
 * Copyright (c) 2007 by Michael Kiefte
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

/*******************************************************************************
 * WARNING: Your map board may be blank or the program may complain that it does
 * not have enough memory. If so go to Preferences->Java->Installed JREs, click
 * on your JRE, hit "edit" and under "Default VM Arguments" put in something
 * like -Xmx256m -Xms256m to increas the RAM allocated to your JVM. Some maps
 * get very big, especially the ones generated from map elements.
 ******************************************************************************/

/* TODO: Future improvements
 * - user interaction to determine tweaks
 * - bezier smoothing of hexlines in hexes with no intersections
 */

package VASSAL.tools.imports.adc2;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.swing.JOptionPane;

import VASSAL.build.GameModule;
import VASSAL.configure.ModuleEditWindow;
import VASSAL.i18n.Resources;
import VASSAL.launch.BasicModule;
import VASSAL.launch.LoadModuleAction;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;
import VASSAL.tools.FileChooser;
import VASSAL.tools.FileFilter;
import VASSAL.tools.imports.FileExtensionFilter;
import VASSAL.tools.imports.FileFormatException;

/**
 * Import a imports module to VASSAL.
 * 
 * @author Michael Kiefte
 *
 */

public class ImportADC2Action extends LoadModuleAction {

	private static final long serialVersionUID = 1L;

	// which zoom level are we importing?
	private int zoomLevel;

	/**
	 * Strip the extension from a filename and replace with the given extension.
	 */
	public static String forceExtension(String s, String ext) {
		if (s.equals(".") || s.equals(".."))
			return s;
		return stripExtension(s) + '.' + ext;
	}

	/**
	 * Get the extension from a file name.
	 */
	public static String getExtension(String s) {
		if (s.equals(".") || s.equals(".."))
			return "";
		final int extIdx = s.lastIndexOf('.');
		final int pathIdx = s.lastIndexOf(File.separatorChar);
		if (extIdx == -1 || extIdx < pathIdx || extIdx >= s.length() - 1)
			return "";
		else
			return s.substring(extIdx + 1);
	}

	/**
	 * Get the file name without the qualifying path.
	 */
	public static String getFileName(String s) {
		if (s.equals(".") || s.equals(".."))
			return s;
		final int pathIdx = s.lastIndexOf(File.separatorChar);
		return s.substring(pathIdx + 1, s.length());
	}

	/**
	 * Get a unique file name for an image in the game module archive. This function
	 * tests the provided name against those that are already present in the archive
	 * and if the file name already exists, proposes an alternate, unique file name.
	 * If an alternate is provided, it is of the form <code><tt>name</tt> + "(<tt>n</tt>)"</code>
	 * where <tt>n</tt> is an integer.
	 */
	public static String getUniqueImageFileName(String s) {
		String t = s;
		int index = 0;
		final ArchiveWriter writer = GameModule.getGameModule().getArchiveWriter();
		while (writer.isImageAdded(DataArchive.IMAGE_DIR + t))
			t = s + '(' + (++index) + ')';
		return t;
	}

	/**
	 * Read a base-250 2-byte big-endian word from a {@link #java.io.DataInputStream 
	 * data input stream}. This is the default encoding for words imported modules.
	 */
	protected static int readBase250Word(DataInputStream in) throws IOException {
		return in.readUnsignedByte() * 250 + in.readUnsignedByte();
	}
	
	/**
	 * Read a base-250 4-byte big-endian word from a {@link #java.io.DataInputStream 
	 * data input stream}. This is the default encoding for integers in imported
	 * modules.
	 */
	protected static int readBase250Integer(DataInputStream in) throws IOException {
		int result = in.readUnsignedByte();
		for (int i = 1; i < 4; ++i)
			result = result*250 + in.readUnsignedByte();
		return result;
	}

	/**
	 * Return a null-terminated string from an input stream.
	 */
	public static String readNullTerminatedString(InputStream in) throws IOException {
		return readNullTerminatedString(in, 0);
	}

	/**
	 * Read a null-terminated string from a file up to a maximum length including
	 * the null termination. If the actual string is longer, no more bytes will be read.
	 */
	public static String readNullTerminatedString(InputStream in, int maxLen) throws IOException {
		final StringBuilder sb;
		if (maxLen == 0)
			sb = new StringBuilder();
		else
			sb = new StringBuilder(maxLen);
		char ch;
		for (int i = 0; maxLen == 0 || i < maxLen; ++i) {
			ch = (char) in.read();
			if (ch != 0)
				sb.append(ch);
			else
				break;
		}
		return sb.toString();
	}

	/**
	 * Read a null-terminated string representing a Windows file name and convert
	 * Windows separator characters <tt>'\\'</tt> to the local separator character.
	 * This is the default file name format for imported modules and should be used
	 * whenever a filename is read as a null-terminated string in order to ensure
	 * platform independence.
	 */
	public static String readWindowsFileName(InputStream in) throws IOException {
		final StringBuilder sb = new StringBuilder();
		char ch;
		do {
			ch = (char) in.read();
			if (ch == '\\')
				sb.append(File.separatorChar);
			else if (ch != 0)
				sb.append(ch);
		} while (ch != 0);
		return sb.toString();
	}

	/**
	 * Return a file name without the extension.
	 */
	public static String stripExtension(String s) {
		if (s.equals(".") || s.equals(".."))
			return s;
		final int index = s.lastIndexOf('.');
		final int pathIdx = s.lastIndexOf(File.separatorChar);
		if (index == -1 || index < pathIdx)
			return s;
		else
			return s.substring(0, index);
	}

	public ImportADC2Action(Component comp) {
		super(comp);
		putValue(NAME, Resources.getString("Main.import_module"));
	}

	protected final static FileFilter bitmapFileFilter = new FileExtensionFilter("bmp", "Bitmap Image");
	protected final static FileFilter setFileFilter = new FileExtensionFilter("set", "ADC Symbol Set");
	protected final static FileFilter mapFileFilter = new FileExtensionFilter("map", "ADC Map Board");
	protected final static FileFilter moduleFileFilter = new FileExtensionFilter("ops", "ADC Game Module");
	
	@Override
	public void performAction(ActionEvent e) throws IOException {
		actionCancelled = true;

		fc.addChoosableFileFilter(setFileFilter);
		fc.addChoosableFileFilter(mapFileFilter);
		fc.setFileFilter(moduleFileFilter);

		if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
			File f = fc.getSelectedFile();
			if (f != null && f.exists()) {
				// can only import one zoom level
				// TODO: in the future, we will always import level 3.
				String[] selectionValues = { "1", "2", "3" };
				String s = (String) JOptionPane.showInputDialog(comp,
						"Zoom level to import:", "Import module",
						JOptionPane.QUESTION_MESSAGE, null, selectionValues,
						selectionValues[2]);
				if (s != null) {
					for (zoomLevel = 0; zoomLevel < 2; ++zoomLevel) {
						if (s == selectionValues[zoomLevel])
							break;
					}

					loadModule(f);
					actionCancelled = false;
				}
			}
		}		
		fc.resetChoosableFileFilters();
	}

	@Override
	protected void loadModule(File f) throws IOException {
		// sort out what to import from the filename extension.
		String ext = getExtension(f.getName()).toLowerCase();
		if (ext.equals(""))
			throw new FileFormatException("Unable to determine file type");

		BasicModule module = new BasicModule(new ArchiveWriter((String) null)); 
		GameModule.init(module);

		if (ext.equals("set"))
			new SymbolSet(this, zoomLevel).read(f).writeToArchive();
		else if (ext.equals("map"))
			new MapBoard(this, zoomLevel).read(f).writeToArchive();
		else if (ext.equals("ops"))
			new ADC2Module(this, zoomLevel).read(f).writeToArchive();
		else
			throw new FileFormatException("Unrecognized filename extension");

		module.getFrame().setVisible(true);
		new ModuleEditWindow().setVisible(true);
	}

	/**
	 * Find case-insensitive, cross-platform match for a given Windows file. Will
	 * ask the user if unable to locate the specified file.
	 */
	public File getCaseInsensitiveFile(File f) {
		return getCaseInsensitiveFile(f, null, true, null);
	}
	
	File getCaseInsensitiveFile(File f, boolean queryIfNotFound) {
		return getCaseInsensitiveFile(f, null, queryIfNotFound, null);
	}

	/**
	 * Find case-insensitive, cross-platform match for a given Windows file. If unable
	 * to find a match, will then search the directory of the second file for a match. 
	 * Will ask the user if still unable to locate the specified file.
	 * 
	 * @param f    File to match with a Windows-specific file-name format.
	 * @param base Another file whose directory to search for a match if unable to find otherwise.
	 * @return     Local match
	 */
	public File getCaseInsensitiveFile(File f, File base) {
		return getCaseInsensitiveFile(f, base, true, null);
	}
	
	File getCaseInsensitiveFile(File f, File base, boolean queryIfNotFound) {
		return getCaseInsensitiveFile(f, base, queryIfNotFound, null);
	}
	
	/**
	 * Find case-insensitive, cross-platform match for a given Windows file. If unable
	 * to find a match, will then search the directory of the second file for a match. 
	 * If still unable to locate the specified file will ask the user to locate the file
	 * using the specified file filter.
	 * 
	 * @param f      File to match with a Windows-specific file-name format.
	 * @param base   Another file whose directory to search for a match.
	 * @param filter <code>FileFilter</code> to use when asking the user to locate the match.
	 * @return       Local match
	 */
	public File getCaseInsensitiveFile(File f, File base, FileFilter filter) {
		return getCaseInsensitiveFile(f, base, true, filter);
	}
	
	/**
	 * Find case-insensitive, cross-platform match for a given Windows file. 
	 * If unable to locate the specified file will ask the user to locate the file
	 * using the specified file filter.
	 * 
	 * @param f      File to match with a Windows-specific file-name format.
	 * @param filter <code>FileFilter</code> to use when asking the user to locate the match.
	 * @return       Local match
	 */
	public File getCaseInsensitiveFile(File f, FileFilter filter) {
		return getCaseInsensitiveFile(f, null, true, filter);
	}
	
	protected File getCaseInsensitiveFile(File f, File base, boolean queryIfNotFound, FileFilter filter) {
		if (f.exists())
			return f;

		final String name = getFileName(f.getName());

		// check files in same directory ignoring case
		final File parent = f.getParentFile();
		if (parent != null) {
			final File[] peers = parent.listFiles();
			if (peers != null) {
				for (File p : peers) {
					if (p.getName().equalsIgnoreCase(name))
						return p;
				}
			}
		}

		// if that doesn't work, check the files in the same directory as "base"
		if (base != null) {
			final File[] peers = base.getParentFile().listFiles();
			if (peers != null) {
				for (File p : peers) {
					if (p.getName().equalsIgnoreCase(name))
						return p;
				}
			}
		}

		// no luck so far.  Ask the user.
		if (queryIfNotFound) {
			JOptionPane.showMessageDialog(comp, "Unable to locate the file:\n"
					+ f.getPath(), "File Warning", JOptionPane.ERROR_MESSAGE);

			fc.resetChoosableFileFilters();
			if (filter != null)
				fc.setFileFilter(filter);
			
			if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
				File p = fc.getSelectedFile();
				if (p.exists())
					return p;
			}
		}

		return null;
	}

	private final static int BLOCK_SEPARATOR = -2;

	/**
	 * Read a block separator byte from an import module file and throw an exception if it
	 * doesn't match.
	 */
	protected static void readBlockHeader(DataInputStream in, String string) throws IOException {
		int header = in.readByte();
		if (header != BLOCK_SEPARATOR)
			throw new FileFormatException("Invalid " + string + " block header");
	}
}
