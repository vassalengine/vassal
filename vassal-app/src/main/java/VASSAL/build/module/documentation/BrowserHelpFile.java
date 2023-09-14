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

import VASSAL.Info;
import VASSAL.build.AbstractBuildable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;
import VASSAL.script.expression.FormattedStringExpression;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.menu.MenuItemProxy;
import VASSAL.tools.menu.MenuManager;

import org.apache.commons.io.file.PathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.swing.AbstractAction;
import javax.swing.Action;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

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
  protected String startingPage = "";
  protected Action launch;
  protected URL url;
  protected File externalTempFile;
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
      // Extract interal Help if it exists, or create an external URL
      extractContents();
    }
    if (url != null) {
      // The starting page is now an expression, so regenerate the URL each time
      url = regenerateUrl();
      BrowserSupport.openURL(url.toString());
    }
  }

  /**
   * @return The entry in the module Zip file containing the HTML directory
   */
  protected String getContentsResource() {
    return name == null ? null : name.replace(' ', '_');
  }

  /** Extract the HTML from the module, or create an External URL from the Starting page if there is none */
  protected void extractContents() {
    try (ZipInputStream in =
           new ZipInputStream(new BufferedInputStream(
             GameModule.getGameModule().getDataArchive().getInputStream(
               "help/" + getContentsResource())))) { //$NON-NLS-1$

      internalExtractContents(in);
    }
    catch (FileNotFoundException | NoSuchFileException e) {
      logger.error("File not found in data archive: {}", "help/" + getContentsResource(), e); //NON-NLS
      setFallbackUrl();
    }
    catch (IOException e) {
      logger.error("Error while reading file {} from data archive", "help/" + getContentsResource(), e); //NON-NLS
      setFallbackUrl();
    }
  }

  /** No HTML found in the module, generate an External URL from the starting page */
  private void setFallbackUrl() {
    try {
      url = new URL(evaluateStartingPage());
    }
    catch (MalformedURLException e) {
      logger.error("Malformed URL: {}", startingPage, e); //NON-NLS
    }
  }

  private void internalExtractContents(ZipInputStream in) throws IOException {
    final Path p = Path.of(
      Info.getTempDir().getAbsolutePath(),
      "VASSAL", //$NON-NLS-1$
      "help",   //$NON-NLS-1$
      getContentsResource()
    );

    if (Files.exists(p)) {
      PathUtils.deleteDirectory(p);
    }

    Files.createDirectories(p);

    ZipEntry entry;
    while ((entry = in.getNextEntry()) != null) {
      if (entry.isDirectory()) {
        Files.createDirectories(p.resolve(entry.getName()));
      }
      else {
// FIXME: no way to distinguish between read and write errors here
        try (OutputStream fos = Files.newOutputStream(p.resolve(entry.getName()))) {
          in.transferTo(fos);
        }
      }
    }
    externalTempFile = p.toFile();
    url = regenerateUrl();
  }

  protected URL regenerateUrl() {
    if (externalTempFile == null) {
      try {
        return new URL(evaluateStartingPage());
      }
      catch (MalformedURLException e) {
        logger.error("Malformed URL: {}", startingPage, e); //NON-NLS
      }
    }
    else {
      try {
        return new File(externalTempFile, evaluateStartingPage()).toURI().toURL();
      }
      catch (MalformedURLException e) {
        logger.error("Malformed URL: {}", startingPage, e); //NON-NLS
      }
    }
    return null;
  }

  protected String evaluateStartingPage() {
    try {
      // Create a shared Audit trail
      final AuditTrail trail = AuditTrail.create(this);
      // Evaluate any $$ variables for old times sake
      final String stage1 = new FormattedStringExpression(startingPage).evaluate(GameModule.getGameModule(), this, trail);
      // Evaluate the resulting expression
      return Expression.createExpression(stage1).evaluate(GameModule.getGameModule(), this, trail);
    }
    catch (ExpressionException e) {
      // Not something that is going to happen often and indicates a broken module setup, so report it properly.
      ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Error.expression_error"),
        "Expression=" + startingPage + ", Error=" + e.getError(), e));
    }
    return startingPage;
  }

  /** @deprecated Use {@link org.apache.commons.io.FileUtils#deleteDirectory(File)} instead. */
  @Deprecated(since = "2020-10-04", forRemoval = true)
  protected void recursiveDelete(File output) {
    if (output.isDirectory()) {
      for (final File f : output.listFiles()) {
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
    MenuManager.getInstance().addToSection("Documentation.Module", launchItem); //NON-NLS
    launch.setEnabled(true);
  }

  @Override
  public void removeFrom(Buildable parent) {
    MenuManager.getInstance()
      .removeFromSection("Documentation.Module", launchItem); //NON-NLS
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
    return HelpFile.getReferenceManualPage("HelpMenu.html", "HtmlHelpFile"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void remove(Buildable child) {
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.BrowserHelpFile.component_type"); //$NON-NLS-1$
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
        Resources.getString("Editor.menu_command"),
        Resources.getString("Editor.BrowserHelpFile.contents"),
        Resources.getString("Editor.BrowserHelpFile.starting_page")
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
        StartPageConfig.class
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
        try (OutputStream fout = Files.newOutputStream(packed.toPath());
             ZipOutputStream out = new ZipOutputStream(fout)) {
          for (final File f : dir.listFiles()) {
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
        for (final File f : packed.listFiles()) {
          packFile(f, prefix + packed.getName() + "/", out); //$NON-NLS-1$
        }
      }
      else {
        final ZipEntry entry = new ZipEntry(prefix + packed.getName());
        out.putNextEntry(entry);

        try (InputStream in = Files.newInputStream(packed.toPath())) {
          in.transferTo(out);
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
    public Configurer getConfigurer() {
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
      return new DirectoryConfigurer(key, name) {
        @Override
        public Component getControls() {
          final Component controls = super.getControls();
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
      myI18nData = new ComponentI18nData(this, "BrowserHelpFile." + getConfigureName(), null, //NON-NLS
        new String[] {TITLE},
        new boolean[] {true},
        new String[] {Resources.getString("Editor.menu_command")});
    }
    return myI18nData;
  }

  public static class StartPageConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      final FormattedExpressionConfigurer configurer = new FormattedExpressionConfigurer(key, name);
      configurer.setValue(((ConfigSupport) c).getAttributeValueString(STARTING_PAGE));
      configurer.setContextLevel(Configurer.ContextLevel.MODULE);
      return configurer;
    }
  }
}
