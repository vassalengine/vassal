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
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.menu.MenuItemProxy;
import VASSAL.tools.menu.MenuManager;

/**
 * Unpacks a zipped directory stored in the module and displays it in an
 * external browser window.
 *
 * @author rkinney
 */
public class BrowserHelpFile extends AbstractBuildable implements Configurable {
  private static final Logger logger =
    LoggerFactory.getLogger(BrowserHelpFile.class);

  public static final String TITLE = "title"; //$NON-NLS-1$
  public static final String CONTENTS = "contents"; //$NON-NLS-1$
  public static final String STARTING_PAGE = "startingPage"; //$NON-NLS-1$
  protected String name;
  protected String startingPage;
  protected Action launch;
  protected URL url;
  protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  protected ComponentI18nData myI18nData;

  public BrowserHelpFile() {
    super();

    launch = new AbstractAction() {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    };
  }

  public void launch() {
    if (url == null) {
      extractContents();
    }
    if (url != null) {
      BrowserSupport.openURL(url.toString());
    }
  }

  /**
   * @return The entry in the module Zip file containing the HTML directory
   */
  protected String getContentsResource() {
    return name == null ? null : name.replace(' ', '_');
  }

  protected void extractContents() {
    try (ZipInputStream in =
           new ZipInputStream(new BufferedInputStream(
             GameModule.getGameModule().getDataArchive().getInputStream(
               "help/" + getContentsResource())))) { //$NON-NLS-1$

      internalExtractContents(in);
    }
    catch (FileNotFoundException e) {
      logger.error("File not found in data archive: {}", "help/" + getContentsResource(), e);
      setFallbackUrl();
    }
    catch (IOException e) {
      logger.error("Error while reading file {} from data archive", "help/" + getContentsResource(), e);
      setFallbackUrl();
    }
  }

  private void setFallbackUrl() {
    try {
      url = new URL(startingPage);
    }
    catch (MalformedURLException e) {
      logger.error("Malformed URL: {}", startingPage, e);
    }
  }

  private void internalExtractContents(ZipInputStream in) throws IOException {
    final File tmp; //$NON-NLS-1$ //$NON-NLS-2$
    tmp = File.createTempFile("VASSAL", "help");
    File output = tmp.getParentFile();
    tmp.delete();
    output = new File(output, "VASSAL"); //$NON-NLS-1$
    output = new File(output, "help"); //$NON-NLS-1$
    output = new File(output, getContentsResource());
    if (output.exists()) recursiveDelete(output);

    output.mkdirs();
    ZipEntry entry;

    while ((entry = in.getNextEntry()) != null) {
      if (entry.isDirectory()) {
        new File(output, entry.getName()).mkdirs();
      }
      else {
// FIXME: no way to distinguish between read and write errors here
        try (FileOutputStream fos = new FileOutputStream(new File(output, entry.getName()))) {
          IOUtils.copy(in, fos);
        }
      }
    }
    url = new File(output, startingPage).toURI().toURL();
  }

  protected void recursiveDelete(File output) {
    if (output.isDirectory()) {
      for (File f : output.listFiles()) {
        recursiveDelete(f);
      }
    }
    else {
      output.delete();
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      TITLE,
      CONTENTS,
      STARTING_PAGE
    };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (TITLE.equals(key)) {
      return name;
    }
    else if (STARTING_PAGE.equals(key)) {
      return startingPage;
    }
    return null;
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (TITLE.equals(key)) {
      name = (String) value;
      launch.putValue(Action.NAME, name);
      url = null;
      getI18nData().setUntranslatedValue(key, name);
    }
    else if (STARTING_PAGE.equals(key)) {
      startingPage = (String) value;
      url = null;
    }
  }

  protected MenuItemProxy launchItem;

  @Override
  public void addTo(Buildable parent) {
    launchItem = new MenuItemProxy(launch);
    MenuManager.getInstance().addToSection("Documentation.Module", launchItem);
    launch.setEnabled(true);
  }

  @Override
  public void removeFrom(Buildable parent) {
    MenuManager.getInstance()
               .removeFromSection("Documentation.Module", launchItem);
    launch.setEnabled(false);
  }

  @Override
  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  @Override
  public String getConfigureName() {
    return name;
  }

  @Override
  public Configurer getConfigurer() {
    return new MyConfigurer(new ConfigSupport());
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HelpMenu.htm","HtmlHelpFile"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void remove(Buildable child) {
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

    @Override
    public String[] getAttributeDescriptions() {
      return new String[]{
        "Menu Entry:  ",
        "Contents:  ",
        "Starting Page:  "
      };
    }

    @Override
    public String[] getAttributeNames() {
      return new String[]{
        TITLE, DIR,
        STARTING_PAGE
      };
    }

    @Override
    public Class<?>[] getAttributeTypes() {
      return new Class<?>[]{
        String.class,
        ContentsConfig.class,
        String.class
      };
    }

    @Override
    public String getAttributeValueString(String key) {
      if (DIR.equals(key)) {
        return dir == null ? null : dir.getPath();
      }
      else {
        return BrowserHelpFile.this.getAttributeValueString(key);
      }
    }

    @Override
    public VisibilityCondition getAttributeVisibility(String name) {
      return null;
    }

    @Override
    public void setAttribute(String key, Object value) {
      if (DIR.equals(key)) {
        dir = (File) value;
      }
      else {
        BrowserHelpFile.this.setAttribute(key, value);
      }
    }

    public void packContents() {
      if (dir == null) return;

      File packed = null;
      try {
        packed = File.createTempFile("VASSALhelp", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
        try (FileOutputStream fout = new FileOutputStream(packed);
             ZipOutputStream out = new ZipOutputStream(fout)) {
          for (File f : dir.listFiles()) {
            packFile(f, "", out); //$NON-NLS-1$
          }
        }

        GameModule.getGameModule().getArchiveWriter().addFile(
          packed.getPath(),
          "help/" + BrowserHelpFile.this.getContentsResource()); //$NON-NLS-1$
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, packed);
      }
    }

    protected void packFile(File packed, String prefix, ZipOutputStream out)
                                                          throws IOException {
      if (packed.isDirectory()) {
        for (File f : packed.listFiles()) {
          packFile(f, prefix + packed.getName() + "/", out); //$NON-NLS-1$
        }
      }
      else {
        final ZipEntry entry = new ZipEntry(prefix + packed.getName());
        out.putNextEntry(entry);

        try (FileInputStream in = new FileInputStream(packed)) {
          IOUtils.copy(in, out);
        }
      }
    }

    @Override
    public void addPropertyChangeListener(PropertyChangeListener l) {
      BrowserHelpFile.this.addPropertyChangeListener(l);
    }

    @Override
    public Class<?>[] getAllowableConfigureComponents() {
      return BrowserHelpFile.this.getAllowableConfigureComponents();
    }

    @Override
    public Configurable[] getConfigureComponents() {
      return BrowserHelpFile.this.getConfigureComponents();
    }

    @Override
    public String getConfigureName() {
      return BrowserHelpFile.this.getConfigureName();
    }

    @Override
    public VASSAL.configure.Configurer getConfigurer() {
      return null;
    }

    @Override
    public HelpFile getHelpFile() {
      return BrowserHelpFile.this.getHelpFile();
    }

    @Override
    public void remove(Buildable child) {
    }

    @Override
    public void removeFrom(Buildable parent) {
    }

    @Override
    public void add(Buildable child) {
    }

    @Override
    public void addTo(Buildable parent) {
    }

    @Override
    public void build(Element e) {
    }

    @Override
    public Element getBuildElement(Document doc) {
      return null;
    }

    @Override
    public ComponentI18nData getI18nData() {
      return null;
    }
  }
  public static class ContentsConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new DirectoryConfigurer(key, name){
        @Override
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

    @Override
    public Object getValue() {
      if (target != null) {
        ((ConfigSupport)target).packContents();
      }
      return super.getValue();
    }
  }

  @Override
  public ComponentI18nData getI18nData() {
    if (myI18nData == null) {
      myI18nData = new ComponentI18nData(this, "BrowserHelpFile." + getConfigureName(), null,
          new String[] {TITLE},
          new boolean[] {true},
          new String[] {"Menu Entry:  "});
    }
    return myI18nData;
  }
}
