/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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

package VASSAL.tools.image.memmap;

import java.awt.image.DataBuffer;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

import VASSAL.tools.io.TempFileManager;

/**
 * A {@link DataBuffer} which stores data internally as integers in
 * a memory-mapped temporary file.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class MappedDataBufferByte extends DataBuffer {
  private ByteBuffer buf = null;
  private final File file;

  private final int bankSize;
  private final int bankNum;

  /**
   * Constructs a byte-based <code>DataBuffer</code> with a single
   * bank and the specified size, with data stored in a memory-mapped
   * temporary file.
   *
   * @param size the size, in <code>int</code>s, of the buffer
   * @throws IOException if the file cannot be created.
   */
  public MappedDataBufferByte(int size, int bankNum) throws IOException {
    super(DataBuffer.TYPE_BYTE, size);

    this.bankSize = size;
    this.bankNum = bankNum;

    file = TempFileManager.getInstance().createTempFile("mem", null); 
    file.deleteOnExit();

    final RandomAccessFile raf = new RandomAccessFile(file, "rw");

    try { 
      buf = raf.getChannel()
               .map(FileChannel.MapMode.READ_WRITE, 0, bankSize*bankNum);
    }
    finally {
      raf.close();
      if (buf == null) dispose();
    }
  }

  /**
   * Attempts to deletes the memory-mapped temporary file which stores this
   * <code>DataBuffer</code>'s data. The <code>DataBuffer</code>
   * <em>must not</em> be used after calling this method.
   */
  public void dispose() {
    // no return value check; TempFileManager will clean up if this fails
    file.delete();
  }

  /**
   * Calls {@link #dispose()}.
   */
  @Override
  protected void finalize() {
    dispose();
  }

  /** {@inheritDoc} */
  @Override
  public int getElem(int bank, int i) {
    if (bank >= bankNum) throw new IllegalArgumentException();
    return (int) buf.get(bank*bankSize+i) & 0xff;
  }

  /** {@inheritDoc} */
  @Override
  public void setElem(int bank, int i, int val) {
    if (bank >= bankNum) throw new IllegalArgumentException();
    buf.put(bank*bankSize+i, (byte) val);
  }
}
