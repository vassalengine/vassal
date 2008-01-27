/*
 * $Id: WriteZipContents.java,v 1.1 2004-02-13 04:57:13 rkinney Exp $
 *
 * Copyright (c) 2004 by Rodney Kinney
 * All rights reserved
 */
package org.vassalengine.ant;

import org.apache.tools.ant.Task;
import org.apache.tools.ant.BuildException;

import java.util.zip.ZipInputStream;
import java.util.zip.ZipEntry;
import java.io.*;

/**
 * Writes a list of entries in a zip file to a destination file
 */
public class WriteZipContents extends Task {
  private String zipFile;
  private String destination;
  public void setZipFile(String s) {
    zipFile = s;
  }

  public void setDestFile(String s) {
    destination = s;
  }

  public void execute() throws BuildException {
    try {
      PrintWriter w = new PrintWriter(new FileWriter(destination));
      ZipInputStream zis = new ZipInputStream(new FileInputStream(zipFile));
      for (ZipEntry entry = zis.getNextEntry(); entry != null;entry = zis.getNextEntry()) {
        if (!entry.isDirectory()) {
          w.println(entry.getName());
        }
      }
      w.close();
    }
    catch (IOException e) {
      throw new BuildException(e);
    }
  }
}
