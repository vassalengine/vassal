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
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import javax.swing.AbstractButton;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileFilter;
import javax.xml.parsers.DocumentBuilderFactory;
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
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import VASSAL.chat.HttpRequestWrapper;

/**
 * Walks the user through a wizard interface. The user may choose between an auto-updating (networked jnlp) or purely
 * local installation (jnlp on local filesystem) installations, and can also select the particular version of VASSAL to install
 * 
 * @author rkinney
 */
public class InstallWizard {
  private File installDir;
  private File installLibDir;
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
//    wiz.setScreen(new InstallerScreen());
    installer = new LocalInstaller();
    chooseVersion();
  }

  public void chooseVersion() {
    wiz.setScreen(new ChooseVersionScreen());
  }

  public void chooseHeapSize() {
    wiz.setScreen(new ChooseHeapSizeScreen());
  }

  public void tryInstall() {
    final InstallProgressScreen screen = new InstallProgressScreen();
    wiz.setScreen(screen);
    new Thread() {
      public void run() {
        try {
          installer.doInstall(screen);
          wiz.setScreen(new SuccessScreen("<html>Installation successful.<br>To get started, double-click on " + installFile+"</html>"));
        }
        catch (IOException e) {
          e.printStackTrace();
          wiz.setScreen(new FailureScreen("Installation failed:  " + e.getMessage()));
        }
        catch (RuntimeException e) {
          e.printStackTrace();
          wiz.setScreen(new FailureScreen("Installation failed:  " + e.getMessage()));
        }
      }
    }.start();    
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
      tf.select(0, tf.getText().length());
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
      SwingUtilities.invokeLater(new Runnable() {
        public void run() {
          tf.requestFocus();
        }});
      return controls;
    }

    public void next() {
      installDir = new File(tf.getText());
      tryInstall();
    }
  }
  private class InstallProgressScreen extends Screen {

    private Box controls;
    private JLabel status;
    private JProgressBar progress;
    public InstallProgressScreen() {
      super();
      controls = Box.createVerticalBox();
      status = new JLabel("Downloading files");
      progress = new JProgressBar();
      progress.setIndeterminate(true);
      controls.add(status);
      controls.add(progress);
    }
    public Component getControls() {
      return controls;
    }

    public void next() {
    }
    public void setStatus(String msg) {
      status.setText(msg);
    }
    
  }
  private static interface Installer {
    public void doInstall(InstallProgressScreen screen) throws IOException;
  }
  private class JnlpInstaller implements Installer {
    protected InstallProgressScreen progress;
    public void doInstall(InstallProgressScreen screen) throws IOException {
      this.progress = screen;
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
        xformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
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
      NodeList l = doc.getElementsByTagName("*");
      for (int i=0,n=l.getLength();i<n;++i) {
        Node node = l.item(i);
        Node child = node.getFirstChild();
        while (child != null) {
          Node next = child.getNextSibling();
          if (child.getNodeType() == Node.TEXT_NODE 
              && child.getNodeValue() != null
              && child.getNodeValue().trim().length() == 0) {
            node.removeChild(child);
          }
          child = next;
        }
      }
      l = doc.getElementsByTagName("j2se");
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
      installLibDir = new File(installDir,"lib");
      if (!installLibDir.exists() && !installLibDir.mkdir()) {
        throw new IOException("Unable to create " + installLibDir);
      }
      if (jnlpURL == null) {
        throw new IOException("No version specified");
      }
      String file = new URL(jnlpURL).getPath();
      file = file.substring(file.lastIndexOf('/')+1);
      installFile = new File(installDir,file);
    }

    public Document getJNLPDoc(URL url) throws IOException {
      Document d;
      try {
        DocumentBuilderFactory f = javax.xml.parsers.DocumentBuilderFactory.newInstance();
        f.setIgnoringElementContentWhitespace(true);
        d = f.newDocumentBuilder().parse(url.openStream());
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
        String href = el.getAttribute("href");
        String url = version == null || version.length() == 0 ? href : href + "?version-id=" + version;
        URL resource = base == null ? new URL(url) : new URL(base, url);
        el.removeAttribute("version");
        el.setAttribute("href", "lib/"+href);
        resources.add(resource);
      }
      NodeList icons = doc.getElementsByTagName("icon");
      for (int i = 0, n = icons.getLength(); i < n; ++i) {
        Element el = (Element) icons.item(i);
        String href = el.getAttribute("href");
        URL resource = base == null ? new URL(href) : new URL(base, href);
        el.setAttribute("href", "lib/"+href);
        resources.add(resource);
      }
      NodeList l = doc.getElementsByTagName("extension");
      for (int i = 0, n = l.getLength(); i < n; ++i) {
        Element el = (Element) l.item(i);
        String href = el.getAttribute("href");
        URL url = base == null ? new URL(href) : new URL(base, href);
        Document extensionDoc = getJNLPDoc(url);
        modifyDocument(extensionDoc);
        String path = getFileName(url);
        el.setAttribute("href", "lib/"+href);
        super.writeDocument(extensionDoc, new File(installLibDir, path));
      }
    }

    public String getFileName(URL url) {
      String path = url.getPath();
      path = path.substring(path.lastIndexOf('/') + 1);
      return path;
    }

    protected void writeDocument(Document doc, File file) throws IOException {
      super.writeDocument(doc, file);
      for (Iterator it = resources.iterator(); it.hasNext();) {
        URL resource = (URL) it.next();
        progress.setStatus("Downloading files: "+getFileName(resource));
        downloadResource(resource);
      }
    }

    private void downloadResource(URL resource) throws IOException {
      byte[] buffer = new byte[100000];
      int readCount = 0;
      String path = getFileName(resource);
      File local = new File(new File(installDir,"lib"), path);
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
        jnlpURL = "http://www.vassalengine.org/ws/vassal.jnlp";
        chooseHeapSize();
      }
      else {
        installer = new LocalInstaller();
        chooseVersion();
      }
    }
  }
  private class SuccessScreen extends Screen {
    private JLabel label;
    private SuccessScreen(String msg) {
      label = new JLabel(msg);
    }
    public Component getControls() {
      return label;
    }
    public void next() {
      System.exit(0);
    }
  }
  private class FailureScreen extends SuccessScreen {
    public FailureScreen(String msg) {
      super(msg);
    }
    public void next() {
      System.exit(1);
    }
  }
  private class ChooseVersionScreen extends Screen {
    private JComboBox choice = new JComboBox();
    private JTextField alternateChoice;
    private Box controls;
    public ChooseVersionScreen() {
      controls = Box.createHorizontalBox();
      controls.add(new JLabel("Select the version to install:  "));
      HttpRequestWrapper req = new HttpRequestWrapper("http://www.vassalengine.org/util/getAllVersionNumbers");
      try {
      DefaultComboBoxModel m = new DefaultComboBoxModel(); 
        for (Enumeration e = req.doGet(null);e.hasMoreElements();) {
          m.insertElementAt(e.nextElement(),0);
        }
        choice.setModel(m);
        controls.add(choice);
        choice.setMaximumSize(new Dimension(choice.getPreferredSize().width,choice.getPreferredSize().height));
        choice.setSelectedIndex(0);
      }
      catch (IOException e) {
        alternateChoice = new JTextField(6);
        controls.add(alternateChoice);
      }
      
    }
    public Component getControls() {
      return controls;
    }
    public void next() {
      if (alternateChoice != null) {
        jnlpURL = "http://www.vassalengine.org/ws/vassal-"+alternateChoice.getText()+".jnlp";
      }
      else {
        jnlpURL = "http://www.vassalengine.org/ws/vassal-"+choice.getSelectedItem()+".jnlp";
      }
      chooseHeapSize();
    }
    
  }
  protected class ChooseHeapSizeScreen extends Screen {
    private static final String HEAP_SIZE = "heapSize";
    private Box controls = Box.createVerticalBox();
    private JRadioButton b128M = new JRadioButton("128 MB");
    private JRadioButton b256M = new JRadioButton("256 MB");
    private JRadioButton b512M = new JRadioButton("512 MB");
    private JRadioButton b1000M = new JRadioButton("1 GB");
    private JRadioButton b1500M = new JRadioButton("1.5 GB");
    private ButtonGroup group = new ButtonGroup();
    
    public ChooseHeapSizeScreen() {
      controls.add(new JLabel("Select memory allocation for the program"));
      b128M.putClientProperty(HEAP_SIZE, "128m");
      group.add(b128M);
      controls.add(b128M);
      b256M.putClientProperty(HEAP_SIZE, "256m");
      group.add(b256M);
      controls.add(b256M);
      b512M.putClientProperty(HEAP_SIZE, "512m");
      group.add(b512M);
      controls.add(b512M);
      b1000M.putClientProperty(HEAP_SIZE, "1000M");
      group.add(b1000M);
      controls.add(b1000M);
      b1500M.putClientProperty(HEAP_SIZE, "1500M");
      group.add(b1500M);
      b256M.setSelected(true);
      controls.add(b1500M);
    }

    public Component getControls() {
      return controls;
    }

    public void next() {
      for (Enumeration e = group.getElements();e.hasMoreElements();) {
        AbstractButton b = (AbstractButton) e.nextElement();
        if (b.isSelected()) {
          maxHeap = (String) b.getClientProperty(HEAP_SIZE);
          break;
        }
      }
      wiz.setScreen(new ChooseDirScreen());
    }
  }
}
