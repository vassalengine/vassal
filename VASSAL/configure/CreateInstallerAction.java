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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.swing.AbstractAction;
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
  private static final Class[] installerClasses = {HttpRequestWrapper.class, ChooseDirScreen.class, ChooseDirScreen.DirectoryFilter.class, ChooseHeapSizeScreen.class, ChooseVersionScreen.class,
                                                   Constants.class, FailureScreen.class, InstallJnlpScreen.class, InstallProgressScreen.class,
                                                   InstallModuleScreen.class, InstallWizard.class, Screen.class, SuccessScreen.class, WizardDialog.class, Resources.class, Resources.VassalPropertyClassLoader.class};
  private Component parent;
  public static final String I18N_PROPERTIES = "VASSAL/i18n/VASSAL.properties";

  public void actionPerformed(ActionEvent e) {
    FileChooser c = FileChooser.createFileChooser(parent);
    if (c.showSaveDialog() == FileChooser.APPROVE_OPTION) {
      File destFile = c.getSelectedFile();
      if (!destFile.getName().endsWith(".jar")) {
        JOptionPane.showMessageDialog(parent, "File name must end in '.jar'");
      }
      else {
        File tempFile;
        try {
          tempFile = File.createTempFile("installer", ".zip");
          ZipOutputStream output = new ZipOutputStream(new FileOutputStream(tempFile));
          writeInstallerClasses(output);
          writeInstallerProperties(output);
          writeManifest(output);
          writeResources(output);
          output.close();
          if (!tempFile.renameTo(destFile)) {
            throw new IOException("Cannot create " + destFile);
          }
        }
        catch (IOException e1) {
          JOptionPane.showMessageDialog(parent, "Error saving file:  " + e1.getMessage());
        }
      }
    }
  }

  private void writeResources(ZipOutputStream output) throws IOException {
    ZipEntry e = new ZipEntry(I18N_PROPERTIES);
    output.putNextEntry(e);
    InputStream input = getClass().getResourceAsStream("/" + I18N_PROPERTIES);
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
    ZipEntry manifestEntry = new ZipEntry("META-INF/MANIFEST.MF");
    manifestEntry.setMethod(ZipEntry.DEFLATED);
    StringBuffer buffer = new StringBuffer();
    buffer.append("Manifest-Version: 1.0\n")
        .append("Main-Class: "+InstallWizard.class.getName()+"\n");
    output.putNextEntry(manifestEntry);
    output.write(buffer.toString().getBytes());
  }

  private void writeInstallerProperties(ZipOutputStream output) throws IOException {
    Properties p = new Properties();
    p.put(Constants.TITLE, "Install " + GameModule.getGameModule().getGameName());
    p.put(Constants.INITIAL_SCREEN, ChooseDirScreen.class.getName());
    p.put(ChooseDirScreen.NEXT_SCREEN, InstallModuleScreen.class.getName());
    p.put(Constants.HEAP_SIZE,"256M");
    String jnlpURL = "http://www.vassalengine.org/ws/vassal-";
    StringTokenizer st = new StringTokenizer(Info.getVersion(), ".b");
    jnlpURL += st.nextToken() + "." + st.nextToken() + ".jnlp";
    p.put(Constants.JNLP_URL, jnlpURL);
    File f = new File(GameModule.getGameModule().getDataArchive().getArchive().getName());
    p.put(Constants.MODULE_FILE, f.getName());
    p.put(Constants.INTERNAL_RESOURCES,I18N_PROPERTIES+","+f.getName());

    ZipEntry e = new ZipEntry(InstallWizard.INSTALL_PROPERTIES);
    output.putNextEntry(e);
    p.store(output, null);
  }

  private void writeInstallerClasses(ZipOutputStream output) throws IOException {
    for (int i = 0; i < installerClasses.length; i++) {
      String className = installerClasses[i].getName().replace('.', '/') + ".class";
      ZipEntry classEntry = new ZipEntry(className);
      classEntry.setMethod(ZipEntry.DEFLATED);
      output.putNextEntry(classEntry);
      InputStream input = getClass().getResourceAsStream("/" + className);
      writeEntry(output, input);
    }
  }

  public static void main(String[] args) throws Exception {
    Resources.init(new File(System.getProperty("user.dir")));
    Prefs globalPrefs = new Prefs(new PrefsEditor(new ArchiveWriter("pref")), "VASSAL");  //$NON-NLS-1$
    GameModule.init(new BasicModule(new ArchiveWriter(args[0]), globalPrefs));
    new CreateInstallerAction().actionPerformed(null);
    System.exit(0);
  }
}
