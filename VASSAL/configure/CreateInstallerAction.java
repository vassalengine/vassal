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
package VASSAL.configure;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.chat.HttpRequestWrapper;
import VASSAL.i18n.Resources;
import VASSAL.launch.BasicModule;
import VASSAL.launch.install.ChooseDirScreen;
import VASSAL.launch.install.ChooseHeapSizeScreen;
import VASSAL.launch.install.ChooseVersionScreen;
import VASSAL.launch.install.Constants;
import VASSAL.launch.install.FailureScreen;
import VASSAL.launch.install.InstallJnlpScreen;
import VASSAL.launch.install.InstallModuleScreen;
import VASSAL.launch.install.InstallProgressScreen;
import VASSAL.launch.install.InstallWizard;
import VASSAL.launch.install.Screen;
import VASSAL.launch.install.SuccessScreen;
import VASSAL.launch.install.WizardDialog;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.FileChooser;

/**
 * @author rkinney
 */
public class CreateInstallerAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private static final Class[] installerClasses = {HttpRequestWrapper.class, ChooseDirScreen.class, ChooseDirScreen.DirectoryFilter.class,
                                                   ChooseHeapSizeScreen.class, ChooseVersionScreen.class, Constants.class, FailureScreen.class,
                                                   InstallJnlpScreen.class, InstallProgressScreen.class, InstallModuleScreen.class, InstallWizard.class,
                                                   Screen.class, SuccessScreen.class, WizardDialog.class, Resources.class,
                                                   Resources.VassalPropertyClassLoader.class};
  private Frame parent;
  public static final String I18N_PROPERTIES = "VASSAL/i18n/VASSAL.properties"; //$NON-NLS-1$
  private static final String[] HEAP_SIZES = new String[]{"256M", "512M", "758M", "1024M", "1536M"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
  private static final String[] HEAP_SIZES_PLAIN_TEXT = new String[]{Resources.getString("Install.256_mb"), Resources.getString("Install.512_mb"), Resources.getString("Install.768_mb"), Resources.getString("Install.1_gb"), Resources.getString("Install.1.5_gb")}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
  private static Map heapSizes = new HashMap();
  static {
    for (int i = 0; i < HEAP_SIZES_PLAIN_TEXT.length; i++) {
      heapSizes.put(HEAP_SIZES_PLAIN_TEXT[i], HEAP_SIZES[i]);
    }
  }
  private String heapSize = "256M"; //$NON-NLS-1$

  public CreateInstallerAction(Frame parent) {
    super(Resources.getString("Install.create_installer")); //$NON-NLS-1$
    this.parent = parent;
  }

  public void actionPerformed(ActionEvent e) {
    final JDialog d = new JDialog(parent, Resources.getString("Install.create_installer"), true); //$NON-NLS-1$
    d.getContentPane().setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
    final StringEnumConfigurer heapSizeConfigurer = new StringEnumConfigurer(null, "Memory Allocation", HEAP_SIZES_PLAIN_TEXT); //$NON-NLS-1$
    heapSizeConfigurer.setValue(HEAP_SIZES_PLAIN_TEXT[0]);
    d.getContentPane().add(heapSizeConfigurer.getControls());
    final JButton ok = new JButton(Resources.getString("General.create")); //$NON-NLS-1$
    JButton cancel = new JButton(Resources.getString("General.cancel")); //$NON-NLS-1$
    Box b = Box.createHorizontalBox();
    b.add(ok);
    b.add(cancel);
    d.getContentPane().add(b);
    d.pack();
    d.setLocationRelativeTo(parent);
    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        d.dispose();
      }
    });
    ok.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        CreateInstallerAction.this.heapSize = (String) heapSizes.get(heapSizeConfigurer.getValue());
        FileChooser c = FileChooser.createFileChooser(parent);
        if (c.showSaveDialog() == FileChooser.APPROVE_OPTION) {
          File destFile = c.getSelectedFile();
          if (!destFile.getName().endsWith(".jar")) { //$NON-NLS-1$
            JOptionPane.showMessageDialog(parent, Resources.getString("Install.require_jar_extension")); //$NON-NLS-1$
          }
          else {
            File tempFile;
            try {
              tempFile = File.createTempFile("installer", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
              ZipOutputStream output = new ZipOutputStream(new FileOutputStream(tempFile));
              writeInstallerClasses(output);
              writeInstallerProperties(output);
              writeManifest(output);
              writeResources(output);
              output.close();
              if (!tempFile.renameTo(destFile)) {
                throw new IOException(Resources.getString("BasicLogger.unable_to_write", destFile.getPath())); //$NON-NLS-1$
              }
            }
            catch (IOException e1) {
              JOptionPane.showMessageDialog(parent, Resources.getString("Install.error_saving_file") + e1.getMessage()); //$NON-NLS-1$
            }
            finally {
              d.dispose();
            }
          }
        }
      }
    });
    d.setVisible(true);
  }

  private void writeResources(ZipOutputStream output) throws IOException {
    ZipEntry e = new ZipEntry(I18N_PROPERTIES);
    output.putNextEntry(e);
    InputStream input = getClass().getResourceAsStream("/" + I18N_PROPERTIES); //$NON-NLS-1$
    writeEntry(output, input);
    File module = new File(GameModule.getGameModule().getDataArchive().getName());
    e = new ZipEntry(module.getName());
    output.putNextEntry(e);
    writeEntry(output, new FileInputStream(module));
  }

  private void writeEntry(ZipOutputStream output, InputStream input) throws IOException {
    byte[] buffer = new byte[1024];
    int n;
    while ((n = input.read(buffer)) > 0) {
      output.write(buffer, 0, n);
    }
  }

  private void writeManifest(ZipOutputStream output) throws IOException {
    ZipEntry manifestEntry = new ZipEntry("META-INF/MANIFEST.MF"); //$NON-NLS-1$
    manifestEntry.setMethod(ZipEntry.DEFLATED);
    StringBuffer buffer = new StringBuffer();
    buffer.append("Manifest-Version: 1.0\n").append("Main-Class: " + InstallWizard.class.getName() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    output.putNextEntry(manifestEntry);
    output.write(buffer.toString().getBytes());
  }

  private void writeInstallerProperties(ZipOutputStream output) throws IOException {
    Properties p = new Properties();
    p.put(Constants.TITLE, "Install " + GameModule.getGameModule().getGameName()); //$NON-NLS-1$
    p.put(Constants.INITIAL_SCREEN, ChooseDirScreen.class.getName());
    p.put(ChooseDirScreen.NEXT_SCREEN, InstallModuleScreen.class.getName());
    p.put(Constants.HEAP_SIZE, heapSize);
    String jnlpURL = "http://www.vassalengine.org/ws/vassal-"; //$NON-NLS-1$
    StringTokenizer st = new StringTokenizer(Info.getVersion(), ".b"); //$NON-NLS-1$
    jnlpURL += st.nextToken() + "." + st.nextToken() + ".jnlp"; //$NON-NLS-1$ //$NON-NLS-2$
    p.put(Constants.JNLP_URL, jnlpURL);
    File f = new File(GameModule.getGameModule().getDataArchive().getArchive().getName());
    p.put(Constants.MODULE_FILE, f.getName());
    p.put(Constants.JNLP_TITLE, GameModule.getGameModule().getGameName());
    p.put(Constants.INTERNAL_RESOURCES, I18N_PROPERTIES + "," + f.getName()); //$NON-NLS-1$
    ZipEntry e = new ZipEntry(InstallWizard.INSTALL_PROPERTIES);
    output.putNextEntry(e);
    p.store(output, null);
  }

  private void writeInstallerClasses(ZipOutputStream output) throws IOException {
    for (int i = 0; i < installerClasses.length; i++) {
      String className = installerClasses[i].getName().replace('.', '/') + ".class"; //$NON-NLS-1$
      ZipEntry classEntry = new ZipEntry(className);
      classEntry.setMethod(ZipEntry.DEFLATED);
      output.putNextEntry(classEntry);
      InputStream input = getClass().getResourceAsStream("/" + className); //$NON-NLS-1$
      writeEntry(output, input);
    }
  }

  public static void main(String[] args) throws Exception {
    Resources.init(new File(System.getProperty("user.dir"))); //$NON-NLS-1$
    Prefs globalPrefs = new Prefs(new PrefsEditor(new ArchiveWriter("pref")), "VASSAL"); //$NON-NLS-1$ //$NON-NLS-2$
    GameModule.init(new BasicModule(new ArchiveWriter(args[0]), globalPrefs));
    new CreateInstallerAction(null).actionPerformed(null);
    System.exit(0);
  }
}
