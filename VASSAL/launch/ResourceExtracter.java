/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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
package VASSAL.launch;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Observer;
import java.util.Properties;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JWindow;
import javax.swing.SwingConstants;
import javax.swing.border.BevelBorder;
import VASSAL.Info;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;
import VASSAL.tools.FileChooser;
import VASSAL.tools.SequenceEncoder;

/**
 * Extracts a Zip file resource to a user-specified directory User: rkinney Date: Dec 5, 2003
 */
public class ResourceExtracter {
  private InstallDirChooser installDirChooser;
  private JProgressBar bar;
  private JWindow monitor;
  private Prefs prefs;
  private Properties props;
  public static final String ZIP_RESOURCE_NAME = "resource"; //$NON-NLS-1$
  public static final String RESOURCE_LIST = "extractList"; //$NON-NLS-1$
  public static final String ASSEMBLE_TARGET = "assembleTo"; //$NON-NLS-1$
  public static final String UPDATE_PROMPT_MSG = "prompt"; //$NON-NLS-1$
  public static final String FIRST_TIME_PROMPT = "initialPrompt"; //$NON-NLS-1$
  public static final String VERSION_ID = "checksum"; //$NON-NLS-1$
  public static final String VERSION_ID_PROPERTY = "checksumKey"; //$NON-NLS-1$
  public static final String INSTALL_DIR_PROPERTY = "installDirKey"; //$NON-NLS-1$
  public static final String INSTALL_DIR_VALUE = "installDir";
  public static final String REQUIRED = "required"; //$NON-NLS-1$
  public static final String STATUS = "status"; //$NON-NLS-1$
  public static final String SUCCESSFUL = "successful"; //$NON-NLS-1$
  public static final String FAILED = "failed"; //$NON-NLS-1$
  private String versionIdKey;
  private JLabel label;
  private File destinationDir;
  private Observer obs;

  public ResourceExtracter(Prefs prefs, Properties props, Observer obs) {
    this.prefs = prefs;
    this.props = props;
    versionIdKey = props.getProperty(VERSION_ID_PROPERTY);
    if (props.getProperty(INSTALL_DIR_VALUE) != null) {
      installDirChooser = new FixedDir(new File(Info.getHomeDir(), props.getProperty(INSTALL_DIR_VALUE)));
    }
    else {
      installDirChooser = new PromptForDir();
    }
    prefs.addOption(null, new StringConfigurer(versionIdKey, null));
    this.obs = obs;
  }

  protected String getPromptMessage() {
    String prompt = isInitialInstall() ? props.getProperty(FIRST_TIME_PROMPT) : props.getProperty(UPDATE_PROMPT_MSG);
    if (prompt == null) {
      return prompt;
    }
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(prompt, '|');
    StringBuffer buffer = new StringBuffer();
    while (st.hasMoreTokens()) {
      buffer.append(st.nextToken());
      if (st.hasMoreTokens()) {
        buffer.append("\n"); //$NON-NLS-1$
      }
    }
    return buffer.toString();
  }

  protected boolean isUpToDate() {
    boolean upToDate = false;
    String checksum = props.getProperty(VERSION_ID, ""); //$NON-NLS-1$
    if (checksum.equals(prefs.getValue(versionIdKey))) {
      File f = installDirChooser.getLastInstallDir();
      if (f != null && f.exists()) {
        if (props.getProperty(REQUIRED) != null) {
          upToDate = new File(f, props.getProperty(REQUIRED)).exists();
        }
        else {
          upToDate = true;
        }
      }
    }
    return upToDate;
  }

  protected boolean isInitialInstall() {
    Object value = prefs.getValue(versionIdKey);
    return value == null || "".equals(value); //$NON-NLS-1$
  }

  /**
   * Initiate full installation procedure: Check version id, prompt for install directory and extract contents in
   * background thread
   * 
   * @throws IOException
   */
  public void install() throws IOException {
    if (isUpToDate()) {
      notifyInstallComplete();
      return;
    }
    if (!confirmInstall()) {
      notifyInstallComplete();
      return;
    }
    destinationDir = installDirChooser.getInstallDir();
    if (destinationDir != null) {
      bar = new JProgressBar(0, 1);
      bar.setAlignmentX(0.0F);
      monitor = new JWindow();
      JPanel panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      panel.setBackground(Color.white);
      label = new JLabel(Resources.getString("ResourceExtracter.installing")); //$NON-NLS-1$
      label.setHorizontalTextPosition(SwingConstants.LEFT);
      label.setBackground(Color.white);
      label.setForeground(Color.black);
      label.setAlignmentX(0.0F);
      panel.add(label);
      panel.add(bar);
      panel.setBorder(new BevelBorder(BevelBorder.RAISED, Color.lightGray, Color.darkGray));
      monitor.getContentPane().add(panel);
      bar.setPreferredSize(new Dimension(400, bar.getPreferredSize().height));
      monitor.pack();
      Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      monitor.setLocation(d.width / 2 - monitor.getWidth() / 2, d.height / 2 - monitor.getHeight() / 2);
      monitor.setVisible(true);
      final IOException[] ex = new IOException[1];
      Runnable runnable = new Runnable() {
        public void run() {
          try {
            extract();
            storeInstallInfo();
            installSucceeded();
          }
          catch (IOException e) {
            ex[0] = e;
          }
        }
      };
      Thread thread = new Thread(runnable);
      thread.start();
      if (ex[0] != null) {
        throw ex[0];
      }
    }
  }

  protected boolean confirmInstall() {
    String prompt = getPromptMessage();
    if (prompt == null) {
      return true;
    }
    if (isInitialInstall()) {
      if (destinationDir == null) {
        JOptionPane.showOptionDialog(null, prompt,
            Resources.getString("ResourceExtracter.install"), JOptionPane.DEFAULT_OPTION //$NON-NLS-1$
            , JOptionPane.QUESTION_MESSAGE, null,
            new String[]{Resources.getString(Resources.SELECT)}, Resources.getString("ResourceExtracter.select")); //$NON-NLS-1$
      }
      return true;
    }
    else {
      return JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(null, prompt, Resources.getString("ResourceExtracter.update"), JOptionPane.YES_NO_OPTION); //$NON-NLS-1$
    }
  }

  protected void storeInstallInfo() throws IOException {
    prefs.getOption(versionIdKey).setValue(props.getProperty(VERSION_ID));
    prefs.write();
  }

  protected void installSucceeded() {
    if (monitor != null) {
      monitor.dispose();
    }
    notifyInstallComplete();
  }

  private void notifyInstallComplete() {
    if (obs != null) {
      obs.update(null, this);
    }
  }

  protected void installFailed(final String msg) {
    monitor.dispose();
  }

  private void extract() throws IOException {
    if (getZipResource() != null) {
      extractZipContents();
    }
    else if (getResourceList() != null) {
      if (getAssembleTarget() != null) {
        packResourceList();
      }
      else {
        extractResourceList();
      }
    }
  }

  /**
   * Extract a list of resources specified by {@link #getResourceList} into the destination directory
   */
  private void extractResourceList() throws IOException {
    label.setText(Resources.getString("ResourceExtracter.unpacking")); //$NON-NLS-1$
    List<String> resources = getListedResources();
    bar.setMaximum(resources.size());
    InputStream in = null;
    int copyCount = 0;
    byte[] buffer = new byte[10000];
    int readCount = 0;
    for (String nextResource : resources) {
      label.setText(nextResource);
      bar.setValue(++copyCount);
      File local = new File(destinationDir, nextResource);
      createDir(local.getParentFile());
      in = getClass().getResourceAsStream("/" + nextResource); //$NON-NLS-1$
      FileOutputStream out = new FileOutputStream(local);
      while ((readCount = in.read(buffer)) > 0) {
        out.write(buffer, 0, readCount);
      }
      out.close();
    }
  }

  protected List<String> getListedResources() throws IOException {
    BufferedReader resourceList = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream(getResourceList())));
    ArrayList<String> resources = new ArrayList<String>();
    String nextResource = null;
    while ((nextResource = resourceList.readLine()) != null) {
      resources.add(nextResource);
    }
    return resources;
  }

  protected String getResourceList() {
    return props.getProperty(RESOURCE_LIST);
  }

  /**
   * Pack the resources specified in {@link #getResourceList} into a Zip file specified by {@link #getAssembleTarget}
   */
  private void packResourceList() throws IOException {
    label.setText(Resources.getString("ResourceExtracter.unpacking")); //$NON-NLS-1$
    File tmp = File.createTempFile("Vdata", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
    ZipOutputStream out = new ZipOutputStream(new FileOutputStream(tmp));
    File target = getAssembleTarget();
    List<String> resources = getListedResources();
    int count = 0;
    bar.setMaximum(resources.size());
    for (String nextResource : resources) {
      label.setText(nextResource);
      byte[] contents = DataArchive.getBytes(getClass().getResourceAsStream("/" + nextResource)); //$NON-NLS-1$
      ZipEntry entry = new ZipEntry(nextResource);
      entry.setMethod(ZipEntry.STORED);
      entry.setSize(contents.length);
      CRC32 checksum = new CRC32();
      checksum.update(contents);
      entry.setCrc(checksum.getValue());
      out.putNextEntry(entry);
      out.write(contents);
      bar.setValue(++count);
    }
    label.setText(Resources.getString("ResourceExtracter.saving")); //$NON-NLS-1$
    out.close();
    if (target.exists()) {
      if (!target.renameTo(getBackupAssembleTarget())) {
        throw new IOException(Resources.getString("ResourceExtracter.unable_to_back_up", target.getPath())); //$NON-NLS-1$
      }
    }
    if (!tmp.renameTo(target)) {
      throw new IOException(Resources.getString("ResourceExtracter.unable_to_create", target.getPath(), tmp.getPath())); //$NON-NLS-1$
    }
  }

  protected File getAssembleTarget() {
    String targetName = props.getProperty(ASSEMBLE_TARGET);
    return targetName != null ? new File(destinationDir, targetName) : null;
  }

  protected File getBackupAssembleTarget() {
    File target = getAssembleTarget();
    String currentVersion = (String) prefs.getValue(versionIdKey);
    if (currentVersion == null) {
      currentVersion = "backup"; //$NON-NLS-1$
    }
    int index = target.getName().lastIndexOf('.');
    String newName = target.getName().substring(0, index) + "-" + currentVersion + target.getName().substring(index); //$NON-NLS-1$
    return new File(target.getParentFile(), newName);
  }

  /** Extract the contents of a Zip file specified by {@link #getZipResource} into the destination directory */
  private void extractZipContents() throws IOException {
    label.setText(Resources.getString("ResourceExtracter.unpacking")); //$NON-NLS-1$
    File tmp = File.createTempFile("Vdata", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
    byte[] buffer = new byte[100000];
    InputStream in = this.getClass().getResourceAsStream(getZipResource());
    OutputStream out = new FileOutputStream(tmp);
    int count = 0;
    while ((count = in.read(buffer)) > 0) {
      out.write(buffer, 0, count);
    }
    in.close();
    out.close();
    ZipInputStream zip = new ZipInputStream(new FileInputStream(tmp));
    bar.setMaximum(new ZipFile(tmp).size());
    ZipEntry entry;
    int entriesCopied = 0;
    while ((entry = zip.getNextEntry()) != null) {
      label.setText(entry.getName());
      bar.setValue(++entriesCopied);
      File local = new File(destinationDir, entry.getName());
      if (entry.isDirectory()) {
        createDir(local);
      }
      else {
        createDir(local.getParentFile());
        out = new FileOutputStream(local);
        count = 0;
        while ((count = zip.read(buffer)) > 0) {
          out.write(buffer, 0, count);
        }
        out.close();
      }
    }
  }

  protected String getZipResource() {
    return props.getProperty(ZIP_RESOURCE_NAME);
  }

  private void createDir(File dir) {
    if (!dir.getParentFile().exists()) {
      createDir(dir.getParentFile());
    }
    if (dir.exists() && !dir.isDirectory()) {
      dir.delete();
    }
    dir.mkdir();
  }
  public static interface InstallDirChooser {
    public File getInstallDir();

    /** The directory from the last install, if any */
    public File getLastInstallDir();
  }
  private class PromptForDir implements InstallDirChooser {
    private String installDirKey;

    private PromptForDir() {
      installDirKey = props.getProperty(INSTALL_DIR_PROPERTY);
      prefs.addOption(null, new DirectoryConfigurer(installDirKey, null));
    }

    /** Prompt user for installation directory */
    public File getInstallDir() {
      FileChooser fc = FileChooser.createFileChooser(null, (DirectoryConfigurer) prefs.getOption(installDirKey), FileChooser.DIRECTORIES_ONLY);
      boolean cancelable = !isInitialInstall();
      File f = null;
      while (f == null) {
        if (fc.showOpenDialog(new JFrame()) == JFileChooser.CANCEL_OPTION && cancelable) {
          f = null;
          break;
        }
        f = fc.getSelectedFile();
        if (f != null) {
          if (!f.exists()) {
            switch (JOptionPane
                .showConfirmDialog(
                    null,
                    Resources.getString("ResourceExtracter.does_not_exist", f.getName()), Resources.getString("ResourceExtracter.create_directory"), JOptionPane.YES_NO_OPTION)) { //$NON-NLS-1$ //$NON-NLS-2$
            case JOptionPane.YES_OPTION:
              if (!f.mkdir()) {
                f = null;
              }
              break;
            case JOptionPane.NO_OPTION:
              f = null;
            }
          }
          else if (!f.isDirectory()) {
            JOptionPane
                .showMessageDialog(
                    null,
                    Resources.getString("ResourceExtracter.is_not_a_directory", f.getName()), Resources.getString("ResourceExtracter.not_a_directory"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            f = null;
          }
        }
      }
      return f;
    }

    public File getLastInstallDir() {
      return (File) prefs.getValue(installDirKey);
    }
  }
  public static class FixedDir implements InstallDirChooser {
    private File installDir;

    public FixedDir(File installDir) {
      super();
      this.installDir = installDir;
    }

    public File getInstallDir() {
      return installDir;
    }

    public File getLastInstallDir() {
      return installDir;
    }
  }

  /*
   * private static long getCrc(InputStream in) throws IOException { CRC32 checksum = new CRC32(); byte[] buffer = new
   * byte[1024]; int count; while ((count = in.read(buffer)) > 0) { checksum.update(buffer, 0, count); } return
   * checksum.getValue(); }
   */
  /*
   * public static void main(String[] args) throws IOException { Properties p = new ArgsParser(args).getProperties();
   * p.put(CHECKSUM, "" + getCrc(new FileInputStream(args[args.length - 1]))); p.put(RESOURCE_NAME, "/" +
   * args[args.length - 1]); p.store(System.out, null); }
   */
  public static void main(String[] args) throws IOException {
    final Properties p = new Properties();
    p.load(new FileInputStream("test")); //$NON-NLS-1$
    p.put(INSTALL_DIR_VALUE, System.getProperty("user.dir"));
    final Prefs prefs = new Prefs(new PrefsEditor(new ArchiveWriter("prefs")), Resources.getString(Resources.VASSAL)); //$NON-NLS-1$
    Runnable runnable = new Runnable() {
      public void run() {
        try {
          new ResourceExtracter(prefs, p, null).install();
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
    };
    runnable.run();
  }
}
