/*
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
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.IOUtils;
import VASSAL.tools.filechooser.FileChooser;

/**
 * @author rkinney
 */
public class CreateInstallerAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private static final Class[] installerClasses = {
    HttpRequestWrapper.class,
    ChooseDirScreen.class,
    ChooseDirScreen.DirectoryFilter.class,
    ChooseHeapSizeScreen.class,
    ChooseVersionScreen.class,
    Constants.class,
    FailureScreen.class,
    InstallJnlpScreen.class,
    InstallProgressScreen.class,
    InstallModuleScreen.class,
    InstallWizard.class,
    Screen.class,
    SuccessScreen.class,
    WizardDialog.class,
    VASSAL.i18n.BundleHelper.class,
    Info.class
  };

  private Frame parent;
  public static final String I18N_PROPERTIES = "VASSAL/i18n/VASSAL.properties"; //$NON-NLS-1$

  private static final String[] HEAP_SIZES = new String[]{
    "256M",  //$NON-NLS-1$
    "512M",  //$NON-NLS-1$
    "758M",  //$NON-NLS-1$
    "1024M", //$NON-NLS-1$
    "1536M"  //$NON-NLS-1$
  };

  private static final String[] HEAP_SIZES_PLAIN_TEXT = new String[]{
    Resources.getString("Install.256_mb"), //$NON-NLS-1$
    Resources.getString("Install.512_mb"), //$NON-NLS-1$
    Resources.getString("Install.768_mb"), //$NON-NLS-1$
    Resources.getString("Install.1_gb"),   //$NON-NLS-1$
    Resources.getString("Install.1.5_gb")  //$NON-NLS-1$
  };
  
  private static Map<String,String> heapSizes = new HashMap<String,String>();

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
    d.setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
    final StringEnumConfigurer heapSizeConfigurer = new StringEnumConfigurer(null, "Memory Allocation", HEAP_SIZES_PLAIN_TEXT); //$NON-NLS-1$
    heapSizeConfigurer.setValue(HEAP_SIZES_PLAIN_TEXT[0]);
    d.add(heapSizeConfigurer.getControls());

    final JButton ok = new JButton(Resources.getString("General.create")); //$NON-NLS-1$
    final JButton cancel = new JButton(Resources.getString("General.cancel")); //$NON-NLS-1$
    final Box b = Box.createHorizontalBox();
    b.add(ok);
    b.add(cancel);
    d.add(b);
    d.pack();
    d.setLocationRelativeTo(parent);

    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        d.dispose();
      }
    });

    ok.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        CreateInstallerAction.this.heapSize =
          heapSizes.get(heapSizeConfigurer.getValue());
        final FileChooser c = FileChooser.createFileChooser(parent);
        if (c.showSaveDialog() == FileChooser.APPROVE_OPTION) {
          final File destFile = c.getSelectedFile();
          if (!destFile.getName().endsWith(".jar")) { //$NON-NLS-1$
            JOptionPane.showMessageDialog(parent, Resources.getString("Install.require_jar_extension")); //$NON-NLS-1$
          }
          else {
            try {
              final File tempFile = File.createTempFile("installer", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
              final ZipOutputStream zipOut =
                new ZipOutputStream(new FileOutputStream(tempFile));
              try {
                writeInstallerClasses(zipOut);
                writeInstallerProperties(zipOut);
                writeManifest(zipOut);
                writeResources(zipOut);
                if (!tempFile.renameTo(destFile)) {
                  throw new IOException(Resources.getString("BasicLogger.unable_to_write", destFile.getPath())); //$NON-NLS-1$
                }
              }
              finally {
                try {
                  zipOut.close();
                }
                catch (IOException ex) {
                  ErrorLog.log(ex);
                }
              }
            }
            catch (IOException ex) {
              JOptionPane.showMessageDialog(parent, Resources.getString("Install.error_saving_file") + ex.getMessage()); //$NON-NLS-1$
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

    InputStream input =
      getClass().getResourceAsStream("/" + I18N_PROPERTIES); //$NON-NLS-1$
    try {
      IOUtils.copy(input, output);
    }
    finally {
      try {
        input.close();
      }
      catch (IOException ex) {
        ErrorLog.log(ex);
      }
    }

    final File module =
      new File(GameModule.getGameModule().getDataArchive().getName());
    e = new ZipEntry(module.getName());
    output.putNextEntry(e);

    input = new FileInputStream(module);
    try {
      IOUtils.copy(input, output);
    }
    finally {
      try {
        input.close();
      }
      catch (IOException ex) {
        ErrorLog.log(ex);
      }
    }
  }

  private void writeManifest(ZipOutputStream output) throws IOException {
    final ZipEntry manifestEntry =
      new ZipEntry("META-INF/MANIFEST.MF"); //$NON-NLS-1$
    manifestEntry.setMethod(ZipEntry.DEFLATED);
    final StringBuilder buffer = new StringBuilder();
    buffer.append("Manifest-Version: 1.0\n")  //$NON-NLS-1$
          .append("Main-Class: " + InstallWizard.class.getName() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
    output.putNextEntry(manifestEntry);
    output.write(buffer.toString().getBytes());
  }

  private void writeInstallerProperties(ZipOutputStream output) throws IOException {
    final Properties p = new Properties();
    p.put(Constants.TITLE, "Install " + GameModule.getGameModule().getGameName()); //$NON-NLS-1$
    p.put(Constants.INITIAL_SCREEN, ChooseDirScreen.class.getName());
    p.put(ChooseDirScreen.NEXT_SCREEN, InstallModuleScreen.class.getName());
    p.put(Constants.HEAP_SIZE, heapSize);
    String jnlpURL = "http://www.vassalengine.org/ws/vassal-"; //$NON-NLS-1$
    jnlpURL += Info.getMinorVersion() + ".jnlp"; //$NON-NLS-1$ //$NON-NLS-2$
    p.put(Constants.JNLP_URL, jnlpURL);
    final File f = new File(GameModule.getGameModule().getDataArchive().getArchive().getName());
    p.put(Constants.MODULE_FILE, f.getName());
    p.put(Constants.JNLP_TITLE, GameModule.getGameModule().getGameName());
    p.put(Constants.INTERNAL_RESOURCES, I18N_PROPERTIES + "," + f.getName()); //$NON-NLS-1$
    final ZipEntry e = new ZipEntry(InstallWizard.INSTALL_PROPERTIES);
    output.putNextEntry(e);
    p.store(output, null);
  }

  private void writeInstallerClasses(ZipOutputStream output) throws IOException {
    for (int i = 0; i < installerClasses.length; i++) {
      final String className = installerClasses[i].getName().replace('.', '/') + ".class"; //$NON-NLS-1$
      final ZipEntry classEntry = new ZipEntry(className);
      classEntry.setMethod(ZipEntry.DEFLATED);
      output.putNextEntry(classEntry);

      final InputStream input =
        getClass().getResourceAsStream("/" + className); //$NON-NLS-1$
      if (input == null)
        throw new IOException("Resource not found: " + className);
      try {
        IOUtils.copy(input, output);
      }
      finally {
        try {
          input.close();
        }
        catch (IOException e) {
          ErrorLog.log(e);
        }
      }
    }
  }

  public static void main(String[] args) throws Exception {
    GameModule.init(new BasicModule(new ArchiveWriter(args[0])));
    new CreateInstallerAction(null).actionPerformed(null);
    System.exit(0);
  }
}
