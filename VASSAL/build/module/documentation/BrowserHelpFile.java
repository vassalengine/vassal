/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.build.module.documentation;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import javax.swing.JMenuItem;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import VASSAL.build.AbstractBuildable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.BrowserSupport;

/**
 * Unpacks a zipped directory stored in the module and displays it in an external browser window
 * 
 * @author rkinney
 */
public class BrowserHelpFile extends AbstractBuildable implements Configurable {
  public static final String TITLE = "title"; //$NON-NLS-1$
  public static final String CONTENTS = "contents"; //$NON-NLS-1$
  public static final String STARTING_PAGE = "startingPage"; //$NON-NLS-1$
  protected String name;
  protected String startingPage;
  protected JMenuItem launch;
  protected URL url;
  protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);

  public BrowserHelpFile() {
    super();
    launch = new JMenuItem();
    launch.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    });
  }

  public void launch() {
    if (url == null) {
      extractContents();
    }
    if (url != null) {
      BrowserSupport.openURL(url);
    }
  }
  
  /**
   * The entry in the module Zip file containing the HTML directory 
   * @return
   */
  protected String getContentsResource() {
    return name == null ? null : name.replace(' ', '_');
  }

  protected void extractContents() {
    try {
      ZipInputStream in = null;
      try {
        in = new ZipInputStream(GameModule.getGameModule().getDataArchive().getFileStream("help/" + getContentsResource())); //$NON-NLS-1$
      }
      catch (IOException e) {
        // The help file was created with empty contents.  Assume an absolute URL as the starting page
        url = new URL(startingPage);
        return;
      }
      File tmp = File.createTempFile("VASSAL", "help"); //$NON-NLS-1$ //$NON-NLS-2$
      File output = tmp.getParentFile();
      tmp.delete();
      output = new File(output, "VASSAL"); //$NON-NLS-1$
      output = new File(output, "help"); //$NON-NLS-1$
      output = new File(output, getContentsResource());
      if (output.exists()) {
        recursiveDelete(output);
      }
      output.mkdirs();
      ZipEntry entry;
      int count;
      byte[] buffer = new byte[1024];
      while ((entry = in.getNextEntry()) != null) {
        if (entry.isDirectory()) {
          new File(output, entry.getName()).mkdirs();
        }
        else {
          OutputStream outStream = new FileOutputStream(new File(output, entry.getName()));
          while ((count = in.read(buffer, 0, 1024)) >= 0) {
            outStream.write(buffer, 0, count);
          }
          outStream.close();
        }
      }
      url = new File(output, startingPage).toURL();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  protected void recursiveDelete(File output) {
    if (output.isDirectory()) {
      File[] dir = output.listFiles();
      for (int i = 0; i < dir.length; i++) {
        recursiveDelete(dir[i]);
      }
    }
    else {
      output.delete();
    }
  }

  public String[] getAttributeNames() {
    return new String[]{TITLE, CONTENTS, STARTING_PAGE};
  }

  public String getAttributeValueString(String key) {
    if (TITLE.equals(key)) {
      return name;
    }
    else if (STARTING_PAGE.equals(key)) {
      return startingPage;
    }
    return null;
  }

  public void setAttribute(String key, Object value) {
    if (TITLE.equals(key)) {
      name = (String) value;
      launch.setText(name);
      url = null;
    }
    else if (STARTING_PAGE.equals(key)) {
      startingPage = (String) value;
      url = null;
    }
  }

  public void addTo(Buildable parent) {
    ((Documentation) parent).getHelpMenu().add(launch);
  }

  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public String getConfigureName() {
    return name;
  }

  public Configurer getConfigurer() {
    return new MyConfigurer(new ConfigSupport());
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HelpMenu.htm","HtmlHelpFile"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void remove(Buildable child) {
  }

  public void removeFrom(Buildable parent) {
    ((Documentation) parent).getHelpMenu().remove(launch);
    launch.setEnabled(false);
  }

  public static String getConfigureTypeName() {
    return "HTML Help File"; //$NON-NLS-1$
  }
  /**
   * The attributes we want to expose in the editor are not the same as the ones we want to save to the buildFile, so we
   * use this object to specify the properties in the editor. Also packs up the contents directory and saves it to the
   * ArchiveWriter
   */
  protected class ConfigSupport implements AutoConfigurable {
    public static final String DIR = "dir"; //$NON-NLS-1$
    protected File dir;

    public String[] getAttributeDescriptions() {
      return new String[]{"Menu Entry", "Contents", "Starting Page"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    public String[] getAttributeNames() {
      return new String[]{TITLE, DIR, STARTING_PAGE};
    }

    public Class[] getAttributeTypes() {
      return new Class[]{String.class, ContentsConfig.class, String.class};
    }

    public String getAttributeValueString(String key) {
      if (DIR.equals(key)) {
        return dir == null ? null : dir.getPath();
      }
      else {
        return BrowserHelpFile.this.getAttributeValueString(key);
      }
    }

    public VisibilityCondition getAttributeVisibility(String name) {
      return null;
    }

    public void setAttribute(String key, Object value) {
      if (DIR.equals(key)) {
        dir = (File) value;
      }
      else {
        BrowserHelpFile.this.setAttribute(key, value);
      }
    }

    public void packContents() {
      if (dir != null) {
        try {
          File packed = File.createTempFile("VASSALhelp", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
          ZipOutputStream out = new ZipOutputStream(new FileOutputStream(packed));
          File[] files = dir.listFiles();
          for (int i = 0; i < files.length; i++) {
            packFile(files[i], "", out); //$NON-NLS-1$
          }
          out.close();
          GameModule.getGameModule().getArchiveWriter().addFile(packed.getPath(), "help/"+BrowserHelpFile.this.getContentsResource()); //$NON-NLS-1$
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
    }

    protected void packFile(File packed, String prefix, ZipOutputStream out) throws IOException {
      if (packed.isDirectory()) {
        ZipEntry entry = new ZipEntry(packed.getName()+"/"); //$NON-NLS-1$
        out.putNextEntry(entry);
        File[] dir = packed.listFiles();
        for (int i = 0; i < dir.length; i++) {
          packFile(dir[i],prefix+packed.getName()+"/",out); //$NON-NLS-1$
        }
      }
      else {
        ZipEntry entry = new ZipEntry(prefix + packed.getName());
        out.putNextEntry(entry);
        InputStream in = new FileInputStream(packed);
        byte[] buffer = new byte[1024];
        int n;
        while ((n = in.read(buffer)) > 0) {
          out.write(buffer, 0, n);
        }
        in.close();
      }
    }

    public void addPropertyChangeListener(PropertyChangeListener l) {
      BrowserHelpFile.this.addPropertyChangeListener(l);
    }

    public Class[] getAllowableConfigureComponents() {
      return BrowserHelpFile.this.getAllowableConfigureComponents();
    }

    public Configurable[] getConfigureComponents() {
      return BrowserHelpFile.this.getConfigureComponents();
    }

    public String getConfigureName() {
      return BrowserHelpFile.this.getConfigureName();
    }

    public VASSAL.configure.Configurer getConfigurer() {
      return null;
    }

    public HelpFile getHelpFile() {
      return BrowserHelpFile.this.getHelpFile();
    }

    public void remove(Buildable child) {
    }

    public void removeFrom(Buildable parent) {
    }

    public void add(Buildable child) {
    }

    public void addTo(Buildable parent) {
    }

    public void build(Element e) {
    }

    public Element getBuildElement(Document doc) {
      return null;
    }
  }
  public static class ContentsConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new DirectoryConfigurer(key, name){
        public Component getControls() {
          Component controls = super.getControls();
          tf.setEditable(false);
          return controls;
        }
      };
    }
  }
  /**
   * Handles the packaging of the target directory into the module file after the user saves the properties in the
   * editor
   * 
   * @author rkinney
   * 
   */
  protected static class MyConfigurer extends AutoConfigurer {
    public MyConfigurer(AutoConfigurable c) {
      super(c);
    }

    public Object getValue() {
      if (target != null) {
        ((ConfigSupport)target).packContents();
      }
      return super.getValue();
    }
  }
}
