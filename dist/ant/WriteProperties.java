/*
 * $Id: WriteProperties.java,v 1.1 2004-02-13 04:57:13 rkinney Exp $
 *
 * Copyright (c) 2004 by Rodney Kinney
 * All rights reserved
 */
package org.vassalengine.ant;

import org.apache.tools.ant.BuildException;

import java.util.Properties;
import java.io.IOException;
import java.io.FileOutputStream;

/**
 * Writes a list of properties to a destination file
 */
public class WriteProperties extends org.apache.tools.ant.Task {
  private Properties props = new Properties();
  private String destFile;

  public void execute() throws BuildException {
    try {
      props.store(new FileOutputStream(destFile), null);
    }
    catch (IOException e) {
      throw new BuildException(e);
    }
  }

  public void setDestFile(String file) {
    destFile = file;
  }

  public void addConfiguredProperty(Property p) {
    props.setProperty(p.getName(), p.getValue());
  }

  public static class Property {
    private String name;
    private String value;

    public String getName() {
      return name;
    }

    public String getValue() {
      return value;
    }

    public void setName(String name) {
      this.name = name;
    }

    public void setValue(String value) {
      this.value = value;
    }
  }
}
