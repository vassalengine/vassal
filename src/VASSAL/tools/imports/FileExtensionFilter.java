package VASSAL.tools.imports;

import java.io.File;

import VASSAL.tools.FileFilter;
import VASSAL.tools.imports.adc2.ImportADC2Action;

/**
 * A simple file filter that does a case-insensitive match against a file name
 * extension.
 */
public class FileExtensionFilter extends FileFilter {
	private final String extension;
	private final String description;
	
	/**
	 * @param extension   file extension
	 * @param description a brief description (<it>e.g.</it>, <tt>"Bitmaps"</tt>).
	 */
	public FileExtensionFilter(String extension, String description) {
		this.extension = extension;
		this.description = description;
	}
	
	public boolean accept(File f) {
		if (f.isDirectory())
			return true;
		if (ImportADC2Action.getExtension(f.getName()).equalsIgnoreCase(extension))
			return true;
		else
			return false;
	}

	public String getDescription() {
		return description + " (*." + extension.toLowerCase() + ";*." + extension.toUpperCase() + ')';
	}
}