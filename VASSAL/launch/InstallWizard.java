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
package VASSAL.launch;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.filechooser.FileFilter;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Walks the user through a wizard interface. The user may choose between an auto-updating (networked jnlp) or purely
 * local installation (jnlp on local filesystem) installations, and can also select the particular version of VASSAL to install
 * 
 * @author rkinney
 */
public class InstallWizard {
  private File installDir;
  private File installFile;
  private String jnlpURL;
  private String maxHeap;
  private Installer installer;
  private Wizard wiz;

  public void start() {
    wiz = new Wizard();
    chooseInstaller();
    wiz.setVisible(true);
  }
  
  public void chooseInstaller() {
    wiz.setScreen(new InstallerScreen());
  }

  public void chooseVersion() {
    jnlpURL = "http://www.vassalengine.org/ws/vassal.jnlp";
    chooseHeapSize();
  }

  public void chooseHeapSize() {
    maxHeap = "256m";
    wiz.setScreen(new ChooseDirScreen());
  }

  public void tryInstall() {
    try {
      installer.doInstall();
      JOptionPane.showMessageDialog(null, "Installation successful.\nTo get started, double-click on " + installFile);
      System.exit(0);
    }
    catch (IOException e) {
      e.printStackTrace();
      JOptionPane.showMessageDialog(null, "Installation failed:  " + e.getMessage());
      System.exit(1);
    }
    catch (RuntimeException e) {
      e.printStackTrace();
      JOptionPane.showMessageDialog(null, "Installation failed:  " + e.getMessage());
      System.exit(1);
    }
  }

  public static void main(String[] args) {
    InstallWizard wiz = new InstallWizard();
    wiz.start();
  }
  
  private static class Wizard extends JDialog {
    private Screen screen;
    private Box screenBox = Box.createHorizontalBox();
    public Wizard() throws HeadlessException {
      super();
      setModal(false);
      setTitle("Install VASSAL");
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          System.exit(0);
        }
      });
      Box buttonBox = Box.createHorizontalBox();
      JButton b = new JButton("Next");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          screen.next();
        }
      });
      buttonBox.add(Box.createHorizontalGlue());
      buttonBox.add(b);
      getContentPane().add("South", buttonBox);
      getContentPane().add(screenBox);
      setSize(600,400);
      setLocationRelativeTo(null);
    }
    public void setScreen(Screen screen) {
      screenBox.removeAll();
      screenBox.add(Box.createVerticalGlue());
      screenBox.add(screen.getControls());
      screenBox.add(Box.createVerticalGlue());
      this.screen = screen;
      validate();
      repaint();
    }
    
  }
  public abstract class Screen {
    public abstract Component getControls();
    public abstract void next();
  }
  private class ChooseDirScreen extends Screen {
    private JTextField tf = new JTextField(36);
    private JButton select = new JButton("Select");
    private Box controls;

    public ChooseDirScreen() {
      Box hBox = Box.createHorizontalBox();
      hBox.add(tf);
      tf.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          next();
        }
      });
      tf.setText(new File(System.getProperty("user.home"),"VASSAL").getPath());
      tf.setMaximumSize(new Dimension(tf.getMaximumSize().width,tf.getPreferredSize().height));
      hBox.add(select);
      select.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          JFileChooser fc = new JFileChooser();
          fc.setFileFilter(new FileFilter() {
            public boolean accept(File pathname) {
              return pathname.isDirectory();
            }

            public String getDescription() {
              return "Directories";
            }
          });
          fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
          if (fc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            tf.setText(fc.getSelectedFile().getPath());
          }
        }
      });
      controls = Box.createVerticalBox();
      controls.add(new JLabel("Select the installation directory"));
      controls.add(hBox);
    }

    public Component getControls() {
      return controls;
    }

    public void next() {
      installDir = new File(tf.getText());
      tryInstall();
    }
  }
  private static interface Installer {
    public void doInstall() throws IOException;
  }
  private class JnlpInstaller implements Installer {
    public void doInstall() throws IOException {
      checkParameters();
      Document doc = getJNLPDoc(new URL(jnlpURL));
      modifyDocument(doc);
      writeDocument(doc, installFile);
    }

    protected void writeDocument(Document doc, File file) throws IOException {
      Writer writer = new FileWriter(file);
      try {
        Source source = new DOMSource(doc);
        // Prepare the output file
        Result result = new StreamResult(writer);
        // Write the DOM document to the file
        Transformer xformer = TransformerFactory.newInstance().newTransformer();
        xformer.setOutputProperty(OutputKeys.INDENT, "yes");
        xformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
        xformer.transform(source, result);
      }
      catch (TransformerException e) {
        throw new IOException(e.getMessage());
      }
      catch (TransformerFactoryConfigurationError e) {
        throw new IOException(e.getMessage());
      }
      writer.close();
    }

    protected void modifyDocument(Document doc) throws IOException {
      NodeList l = doc.getElementsByTagName("j2se");
      if (l.getLength() == 1) {
        Element el = (Element) l.item(0);
        el.setAttribute("max-heap-size", maxHeap);
      }
    }

    protected void checkParameters() throws IOException {
      if (installDir == null) {
        throw new IOException("Installation directory not specified");
      }
      if (installDir.exists() && !installDir.isDirectory()) {
        throw new IOException(installDir + " is not a directory");
      }
      if (!installDir.exists() && !installDir.mkdir()) {
        throw new IOException("Unable to create " + installDir);
      }
      if (jnlpURL == null) {
        throw new IOException("No version specified");
      }
      installFile = new File(installDir,new URL(jnlpURL).getFile());
    }

    public Document getJNLPDoc(URL url) throws IOException {
      Document d;
      try {
        d = javax.xml.parsers.DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(url.openStream());
      }
      catch (SAXException e) {
        throw new IOException("SAXException:  " + e.getMessage());
      }
      catch (ParserConfigurationException e) {
        throw new IOException("ParserConfigurationException:  " + e.getMessage());
      }
      catch (FactoryConfigurationError e) {
        throw new IOException("FactoryConfigurationError:  " + e.getMessage());
      }
      return d;
    }
  }
  private class LocalInstaller extends JnlpInstaller {
    private List resources = new ArrayList();

    protected void modifyDocument(Document doc) throws IOException {
      extractResources(doc);
      super.modifyDocument(doc);
      doc.getDocumentElement().setAttribute("codebase", installDir.toURL().toString());
    }

    private void extractResources(Document doc) throws IOException {
      String codebase = doc.getDocumentElement().getAttribute("codebase");
      URL base = null;
      if (codebase != null) {
        if (!codebase.endsWith("/")) {
          codebase += "/";
        }
        base = new URL(codebase);
      }
      NodeList jars = doc.getElementsByTagName("jar");
      for (int i = 0, n = jars.getLength(); i < n; ++i) {
        Element el = (Element) jars.item(i);
        String version = el.getAttribute("version");
        String url = version == null || version.length() == 0 ? el.getAttribute("href") : el.getAttribute("href") + "?version-id=" + version;
        URL resource = base == null ? new URL(url) : new URL(base, url);
        el.setAttribute("version", "");
        resources.add(resource);
      }
      NodeList l = doc.getElementsByTagName("extension");
      for (int i = 0, n = l.getLength(); i < n; ++i) {
        Element el = (Element) l.item(i);
        String url = el.getAttribute("href");
        URL extURL = base == null ? new URL(url) : new URL(base, url);
        Document extensionDoc = getJNLPDoc(extURL);
        modifyDocument(extensionDoc);
        String path = extURL.getPath();
        path = path.substring(path.lastIndexOf('/') + 1);
        writeDocument(extensionDoc, new File(installDir, path));
      }
    }

    protected void writeDocument(Document doc, File file) throws IOException {
      super.writeDocument(doc, file);
      for (Iterator it = resources.iterator(); it.hasNext();) {
        URL resource = (URL) it.next();
        downloadResource(resource);
      }
    }

    private void downloadResource(URL resource) throws IOException {
      byte[] buffer = new byte[100000];
      int readCount = 0;
      String path = resource.getPath();
      path = path.substring(path.lastIndexOf('/') + 1);
      File local = new File(installDir, path);
      FileOutputStream out = new FileOutputStream(local);
      InputStream in = resource.openStream();
      while ((readCount = in.read(buffer)) > 0) {
        out.write(buffer, 0, readCount);
      }
      out.close();
    }
  }
  private class AutoUpdateInstaller extends JnlpInstaller {
  }
  private class InstallerScreen extends Screen {
    private Box b = Box.createVerticalBox();
    private JRadioButton auto = new JRadioButton("Networked installation (updates automatically)");
    private JRadioButton local = new JRadioButton("Local installation");
    public InstallerScreen() {
      super();
      ButtonGroup g = new ButtonGroup();
      g.add(auto);
      g.add(local);
      auto.setSelected(true);
      b.add(new JLabel("Choose type of installation:"));
      b.add(auto);
      b.add(local);
    }

    public Component getControls() {
      return b;
    }

    public void next() {
      if (auto.isSelected()) {
        installer = new AutoUpdateInstaller();
      }
      else {
        installer = new LocalInstaller();
      }
      chooseVersion();
    }
  }
}
