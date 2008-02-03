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

package VASSAL.tools.imports.adc2;

import java.io.DataInputStream;
import java.io.IOException;

import VASSAL.tools.imports.FileFormatException;

/**
 * Common utilities for importing ADC2 modules to VASSAL.
 * 
 * @author Michael Kiefte
 *
 */

class ADC2Utils {
	
	// can never be instantiated
	private ADC2Utils() {}

	private static final long serialVersionUID = 1L;

	/**
	 * Read a base-250 2-byte big-endian word from a <code>DataInputStream</code>. 
	 * This is the default (and only) encoding for words imported modules.
	 */
	static int readBase250Word(DataInputStream in) throws IOException {
		return in.readUnsignedByte() * 250 + in.readUnsignedByte();
	}
	
	/**
	 * Read a base-250 4-byte big-endian word from a <code>DataInputStream</code>. 
	 * This is the default encoding for integers in ADC2 modules (why?).
	 */
	static int readBase250Integer(DataInputStream in) throws IOException {
		return readBase250Word(in) * 62500 + readBase250Word(in);
	}

	final static int BLOCK_SEPARATOR = -2;

	/**
	 * Read a block separator byte from an import module file and throw an exception if it
	 * doesn't match.
	 */
	static void readBlockHeader(DataInputStream in, String string) throws IOException {
		int header = in.readByte();
		if (header != BLOCK_SEPARATOR)
			throw new FileFormatException("Invalid " + string + " block header in.");
	}
}
