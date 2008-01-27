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

package VASSAL.tools.memmap;

import java.awt.image.DataBuffer;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A {@link DataBuffer} which stores data internally as integers in
 * a memory-mapped temporary file.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class MappedDataBufferInt extends DataBuffer {
  private MappedByteBuffer buf = null;
  private final File file;

/*
  private static final Set<MappedDataBufferInt> instances =
    Collections.newSetFromMap(
      new ConcurrentHashMap<MappedDataBufferInt,Boolean>());
*/

  private static final Set<File> tmpFiles =
    Collections.newSetFromMap(new ConcurrentHashMap<File,Boolean>());

  static {
    Runtime.getRuntime().addShutdownHook(new Thread() {
      public void run() {
/*
        for (MappedDataBufferInt buf : instances) {
          try {
            buf.dispose();
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
*/
// Workaround for Java Bug #4724038
        long sleep = 1;
        final long maxsleep = 10000;
        boolean success = false;

        while (!success) {
          success = true;
          for (Iterator<File> i = tmpFiles.iterator(); i.hasNext(); ) {
            if (i.next().delete()) i.remove();
            else success = false;
          }

          if (!success) {
            if (sleep > maxsleep) {
              System.out.println("Can't delete temp files, giving up!");
              return;
            } 

            try { Thread.sleep(sleep); }
            catch (InterruptedException e) { }

            System.gc();
            System.runFinalization();
            sleep *= 2;
          }
        }
      }
    });
  }

  /**
   * Constructs an integer-based <code>DataBuffer</code> with a single
   * bank and the specified size, with data stored in a memory-mapped
   * temporary file.
   *
   * @param size the size, in <code>int</code>s, of the buffer
   * @throws IOException if the file cannot be created.
   */
  public MappedDataBufferInt(int size) throws IOException {
    super(DataBuffer.TYPE_INT, size);

    file = File.createTempFile("mem",
                               null,
                               new File(System.getProperty("user.dir")));
//    file.deleteOnExit();
    // System.out.println(file.getAbsolutePath());

    final RandomAccessFile raf = new RandomAccessFile(file, "rw");

    try { 
      buf = raf.getChannel().map(FileChannel.MapMode.READ_WRITE, 0, 4*size);
//      instances.add(this);
      tmpFiles.add(file);      
    }
    finally {
      raf.close();
      if (buf == null) dispose();
    }
  }

  /**
   * Closes and deletes the memory-mapped temporary file which stores this
   * <code>DataBuffer</code>'s data. The <code>DataBuffer</code> cannot be
   * used after calling this method.
   *
   * @throws IOException if the file cannot be closed or deleated.
   */
  public void dispose() throws IOException {
// FIXME: what happens if other methods are called after dispose()?
// FIXME: needn't throw IOException now? 
    file.delete();
//    instances.remove(this);
    tmpFiles.remove(file);
  }

  @Override
  protected void finalize() throws IOException {
// FIXME: This seems not to be called on app close!
    dispose();
  }

  
  /** {@inheritDoc} */
  @Override
  public int getElem(int bank, int i) {
    if (bank != 0) throw new IllegalArgumentException();
    return buf.getInt(4*i);
  }

  /** {@inheritDoc} */
  @Override
  public void setElem(int bank, int i, int val) {
    if (bank != 0) throw new IllegalArgumentException();
    buf.putInt(4*i, val);
  }
}
