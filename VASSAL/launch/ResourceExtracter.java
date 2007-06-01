package VASSAL.launch;

import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.DataArchive;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.zip.*;

/**
 * Extracts a Zip file resource to a user-specified directory
 * User: rkinney
 * Date: Dec 5, 2003
 */
public class ResourceExtracter {
  private File destinationDir;
  private JProgressBar bar;
  private JWindow monitor;
  private Prefs prefs;
  private Properties props;
  public static final String ZIP_RESOURCE_NAME = "resource";  //$NON-NLS-1$
  public static final String RESOURCE_LIST = "extractList";  //$NON-NLS-1$
  public static final String ASSEMBLE_TARGET = "assembleTo";  //$NON-NLS-1$
  public static final String UPDATE_PROMPT_MSG = "prompt";  //$NON-NLS-1$
  public static final String FIRST_TIME_PROMPT = "initialPrompt";  //$NON-NLS-1$
  public static final String VERSION_ID = "checksum";  //$NON-NLS-1$
  public static final String VERSION_ID_PROPERTY = "checksumKey";  //$NON-NLS-1$
  public static final String INSTALL_DIR_PROPERTY = "installDirKey";  //$NON-NLS-1$
  public static final String REQUIRED = "required";  //$NON-NLS-1$
  public static final String STATUS = "status";  //$NON-NLS-1$
  public static final String SUCCESSFUL = "successful";  //$NON-NLS-1$
  public static final String FAILED = "failed";  //$NON-NLS-1$
  private PropertyChangeListener listener;
  private String versionIdKey;
  private String installDirKey;
  private JLabel label;

  public ResourceExtracter(Prefs prefs, Properties props, PropertyChangeListener listener) {
    this(prefs, props, listener, null);
  }

  public ResourceExtracter(Prefs prefs, Properties props, PropertyChangeListener listener, File installDir) {
    this.destinationDir = installDir;
    this.listener = listener;
    this.prefs = prefs;
    this.props = props;
    versionIdKey = props.getProperty(VERSION_ID_PROPERTY);
    if (versionIdKey == null) {
      throw new IllegalArgumentException(Resources.getString("ResourceExtracter.must_specify", VERSION_ID_PROPERTY));  //$NON-NLS-1$
    }
    installDirKey = props.getProperty(INSTALL_DIR_PROPERTY);
    if (installDirKey == null) {
      throw new IllegalArgumentException(Resources.getString("ResourceExtracter.must_specify", INSTALL_DIR_PROPERTY));  //$NON-NLS-1$
    }
    prefs.addOption(null, new StringConfigurer(versionIdKey, null));
    DirectoryConfigurer config = new DirectoryConfigurer(installDirKey, null);
    config.setValue((Object) null);
    prefs.addOption(null, config);
  }

  /** Prompt user for installation directory */
  protected File chooseInstallDir() {
    JFileChooser chooser = new JFileChooser(System.getProperty("user.home"));  //$NON-NLS-1$
    chooser.setSelectedFile((File) prefs.getValue(installDirKey));
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    boolean cancelable = !isInitialInstall();
    File f = null;
    while (f == null) {
      if (chooser.showOpenDialog(null) == JFileChooser.CANCEL_OPTION
          && cancelable) {
        f = null;
        break;
      }
      f = chooser.getSelectedFile();
      if (f != null) {
        if (!f.exists()) {
          switch (JOptionPane.showConfirmDialog(null, Resources.getString("ResourceExtracter.does_not_exist", f.getName()), Resources.getString("ResourceExtracter.create_directory"), JOptionPane.YES_NO_OPTION)) {   //$NON-NLS-1$ //$NON-NLS-2$
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
          JOptionPane.showMessageDialog(null, Resources.getString("ResourceExtracter.is_not_a_directory", f.getName()), Resources.getString("ResourceExtracter.not_a_directory"), JOptionPane.ERROR_MESSAGE);   //$NON-NLS-1$ //$NON-NLS-2$
          f = null;
        }
      }
    }
    return f;
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
        buffer.append("\n");  //$NON-NLS-1$
      }
    }
    return buffer.toString();
  }

  protected boolean isUpToDate() {
    boolean upToDate = false;
    String checksum = props.getProperty(VERSION_ID, "");  //$NON-NLS-1$
    if (checksum.equals(prefs.getValue(versionIdKey))) {
      File f = (File) prefs.getValue(installDirKey);
      if (f != null
          && f.exists()) {
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
    return value == null || "".equals(value);  //$NON-NLS-1$
  }

  /**
   * Initiate full installation procedure:  Check version id, prompt for install directory
   * and extract contents in background thread
   */
  public void install() {
    if (isUpToDate()) {
      installSkipped();
      return;
    }
    if (!confirmInstall()) {
      installSkipped();
      return;
    }
    if (destinationDir == null) {
      File lastInstallDir = (File) prefs.getValue(installDirKey);
      if (lastInstallDir != null
          && lastInstallDir.exists()
          && lastInstallDir.isDirectory()) {
        destinationDir = lastInstallDir;
      }
      else {
        destinationDir = chooseInstallDir();
      }
    }
    if (destinationDir != null) {
      bar = new JProgressBar(0, 1);
      bar.setPreferredSize(new Dimension(400, bar.getPreferredSize().height));
      bar.setAlignmentX(0.0F);
      monitor = new JWindow();
      JPanel panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      panel.setBackground(Color.white);
      label = new JLabel(Resources.getString("ResourceExtracter.installing"));  //$NON-NLS-1$
      label.setHorizontalTextPosition(SwingConstants.LEFT);
      label.setBackground(Color.white);
      label.setForeground(Color.black);
      label.setAlignmentX(0.0F);
      panel.add(label);
      panel.add(bar);
      panel.setBorder(new BevelBorder(BevelBorder.RAISED, Color.lightGray, Color.darkGray));
      monitor.getContentPane().add(panel);
      monitor.pack();
      Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      monitor.setLocation(d.width / 2 - monitor.getWidth() / 2, d.height / 2 - monitor.getHeight() / 2);
      monitor.setVisible(true);
      Runnable runnable = new Runnable() {
        public void run() {
          try {
            extract();
            storeInstallInfo();
            installSucceeded();
          }
          catch (IOException e) {
            e.printStackTrace();
            String msg = e.getMessage();
            if (msg == null
                || msg.length() == 0) {
              msg = e.getClass().getName().substring(e.getClass().getName().lastIndexOf("."));  //$NON-NLS-1$
            }
            installFailed(msg);
          }
        }
      };
      new Thread(runnable).start();
    }
    else {
      installSkipped();
    }
  }

  protected boolean confirmInstall() {
    String prompt = getPromptMessage();
    if (prompt == null) {
      return true;
    }
    if (isInitialInstall()) {
      if (destinationDir == null) {
        JOptionPane.showOptionDialog(null, prompt, Resources.getString("ResourceExtracter.install"), JOptionPane.DEFAULT_OPTION  //$NON-NLS-1$
                                     , JOptionPane.QUESTION_MESSAGE, null, new String[]{Resources.getString("ResourceExtracter.select")}, Resources.getString("ResourceExtracter.select"));   //$NON-NLS-1$ //$NON-NLS-2$
      }
      return true;
    }
    else {
      return JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(null, prompt, Resources.getString("ResourceExtracter.update"), JOptionPane.YES_NO_OPTION);  //$NON-NLS-1$
    }
  }

  protected void storeInstallInfo() throws IOException {
    prefs.getOption(versionIdKey).setValue(props.getProperty(VERSION_ID));
    prefs.getOption(installDirKey).setValue(destinationDir);
    prefs.write();
  }

  protected void installSucceeded() {
    Runnable runnable = new Runnable() {
      public void run() {
        if (monitor != null) {
          monitor.dispose();
        }
        String msg = isInitialInstall() ? Resources.getString("ResourceExtracter.install_complete") : Resources.getString("ResourceExtracter.update_complete");   //$NON-NLS-1$ //$NON-NLS-2$
        JOptionPane.showMessageDialog(null, msg, Resources.getString("ResourceExtracter.success"), JOptionPane.INFORMATION_MESSAGE);  //$NON-NLS-1$
        listener.propertyChange(new PropertyChangeEvent(this, STATUS, null, SUCCESSFUL));
      }
    };
    SwingUtilities.invokeLater(runnable);
  }

  protected void installSkipped() {
    Runnable runnable = new Runnable() {
      public void run() {
        listener.propertyChange(new PropertyChangeEvent(this, STATUS, null, SUCCESSFUL));
      }
    };
    SwingUtilities.invokeLater(runnable);
  }

  protected void installFailed(final String msg) {
    Runnable runnable = new Runnable() {
      public void run() {
        monitor.dispose();
        JOptionPane.showMessageDialog(null, Resources.getString("ResourceExtracter.unable_to_install", msg), Resources.getString("ResourceExtracter.install_failed"), JOptionPane.ERROR_MESSAGE);   //$NON-NLS-1$ //$NON-NLS-2$
        listener.propertyChange(new PropertyChangeEvent(this, STATUS, null, FAILED));
      }
    };
    SwingUtilities.invokeLater(runnable);
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

  /** Extract a list of resources specified by {@link #getResourceList} into the destination directory */
  private void extractResourceList() throws IOException {
    label.setText(Resources.getString("ResourceExtracter.unpacking"));  //$NON-NLS-1$
    List resources = getListedResources();
    String nextResource;
    bar.setMaximum(resources.size());
    InputStream in = null;
    int copyCount = 0;
    byte[] buffer = new byte[10000];
    int readCount = 0;
    for (Iterator it = resources.iterator(); it.hasNext();) {
      nextResource = (String) it.next();
      label.setText(nextResource);
      bar.setValue(++copyCount);
      File local = new File(destinationDir, nextResource);
      createDir(local.getParentFile());
      in = getClass().getResourceAsStream("/" + nextResource);  //$NON-NLS-1$
      FileOutputStream out = new FileOutputStream(local);
      while ((readCount = in.read(buffer)) > 0) {
        out.write(buffer, 0, readCount);
      }
      out.close();
    }
  }

  protected List getListedResources() throws IOException {
    BufferedReader resourceList = new BufferedReader(
      new InputStreamReader(getClass().getResourceAsStream(getResourceList())));
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

  /** Pack the resources specified in {@link #getResourceList} into a Zip file specified by {@link #getAssembleTarget} */
  private void packResourceList() throws IOException {
    label.setText(Resources.getString("ResourceExtracter.unpacking"));  //$NON-NLS-1$
    File tmp = File.createTempFile("Vdata", ".zip");   //$NON-NLS-1$ //$NON-NLS-2$
    ZipOutputStream out =
        new ZipOutputStream(new FileOutputStream(tmp));
    File target = getAssembleTarget();
    List resources = getListedResources();
    int count = 0;
    bar.setMaximum(resources.size());
    for (Iterator it = resources.iterator(); it.hasNext();) {
      String nextResource = (String) it.next();
      label.setText(nextResource);
      byte[] contents = DataArchive.getBytes(getClass().getResourceAsStream("/" + nextResource));  //$NON-NLS-1$
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
    label.setText(Resources.getString("ResourceExtracter.saving"));  //$NON-NLS-1$
    out.close();
    if (target.exists()) {
      if (!target.renameTo(getBackupAssembleTarget())) {
        throw new IOException(Resources.getString("ResourceExtracter.unable_to_back_up", target.getPath()));  //$NON-NLS-1$
      }
    }
    if (!tmp.renameTo(target)) {
      throw new IOException(Resources.getString("ResourceExtracter.unable_to_create", target.getPath(), tmp.getPath()));   //$NON-NLS-1$
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
      currentVersion = "backup";  //$NON-NLS-1$
    }
    int index = target.getName().lastIndexOf('.');
    String newName = target.getName().substring(0, index)
        + "-" + currentVersion + target.getName().substring(index);  //$NON-NLS-1$
    return new File(target.getParentFile(), newName);
  }

  /** Extract the contents of a Zip file specified by {@link #getZipResource} into the destination directory */
  private void extractZipContents() throws IOException {
    label.setText(Resources.getString("ResourceExtracter.unpacking"));  //$NON-NLS-1$
    File tmp = File.createTempFile("Vdata", ".zip");   //$NON-NLS-1$ //$NON-NLS-2$
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
    if (dir.exists()
        && !dir.isDirectory()) {
      dir.delete();
    }
    dir.mkdir();
  }

/*
  private static long getCrc(InputStream in) throws IOException {
    CRC32 checksum = new CRC32();
    byte[] buffer = new byte[1024];
    int count;
    while ((count = in.read(buffer)) > 0) {
      checksum.update(buffer, 0, count);
    }
    return checksum.getValue();
  }
*/

/*
  public static void main(String[] args) throws IOException {
    Properties p = new ArgsParser(args).getProperties();
    p.put(CHECKSUM, "" + getCrc(new FileInputStream(args[args.length - 1])));
    p.put(RESOURCE_NAME, "/" + args[args.length - 1]);
    p.store(System.out, null);
  }
*/
  public static void main(String[] args) throws IOException {
    final Properties p = new Properties();
    p.load(new FileInputStream("test"));  //$NON-NLS-1$
    final Prefs prefs = new Prefs(new PrefsEditor(new ArchiveWriter("prefs")), Resources.getString(Resources.VASSAL));   //$NON-NLS-1$
    Runnable runnable = new Runnable() {
      public void run() {
        new ResourceExtracter(prefs, p, new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            System.out.println("" + evt.getNewValue());  //$NON-NLS-1$
          }
        }).install();
      }
    };
    runnable.run();
  }
}
