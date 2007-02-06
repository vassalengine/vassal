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

import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.URL;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
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
 * @author rkinney
 */
public class InstallWizard {
  private File installDir;
  private String jnlpURL;
  private String maxHeap;
  private File launchFile;

  public void startWizard() {
    chooseVersion();
  }

  public void chooseVersion() {
    jnlpURL = "http://www.vassalengine.org/ws/vassal.jnlp";
    chooseHeapSize();
  }

  public void chooseHeapSize() {
    maxHeap = "256m";
    new ChooseDirScreen();
  }

  public void tryInstall() {
    try {
      doInstall();
      JOptionPane.showMessageDialog(null, "Installation successful.\nTo get started, double-click on " + launchFile);
    }
    catch (IOException e) {
      JOptionPane.showMessageDialog(null, "Installation failed:  " + e.getMessage());
      System.exit(1);
    }
    catch (RuntimeException e) {
      JOptionPane.showMessageDialog(null, "Installation failed:  " + e.getMessage());
      System.exit(1);
    }
  }

  public void doInstall() throws IOException {
    if (installDir == null) {
      throw new IOException("Installation directory not specified");
    }
    if (installDir.exists() && !installDir.isDirectory()) {
      throw new IOException(installDir + " is not a directory");
    }
    if (!installDir.exists() && !installDir.mkdir()) {
      throw new IOException("Unable to create " + installDir);
    }
    Document doc = getJNLPDoc();
    NodeList l = doc.getElementsByTagName("j2se");
    if (l.getLength() != 1) {
      throw new IOException("Badly formed jnlp file at " + jnlpURL);
    }
    Element el = (Element) l.item(0);
    el.setAttribute("max-heap-size", maxHeap);
    launchFile = new File(installDir, "vassal.jnlp");
    Writer writer = new FileWriter(launchFile);
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

  public Document getJNLPDoc() throws IOException {
    Document d;
    try {
      d = javax.xml.parsers.DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new URL(jnlpURL).openStream());
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

  public static void main(String[] args) {
    InstallWizard wiz = new InstallWizard();
    wiz.startWizard();
  }
  private abstract static class Screen extends JDialog {
    public Screen() throws HeadlessException {
      super();
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      Box box = Box.createHorizontalBox();
      JButton b = new JButton("Next");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          next();
        }
      });
      box.add(Box.createHorizontalGlue());
      box.add(b);
      getContentPane().add("South", box);
    }

    public abstract void next();
  }
  private class ChooseDirScreen extends Screen {
    private JTextField tf = new JTextField(36);
    private JButton select = new JButton("Select");

    public ChooseDirScreen() throws HeadlessException {
      super();
      Box hBox = Box.createHorizontalBox();
      hBox.add(tf);
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
      Box vBox = Box.createVerticalBox();
      vBox.add(new JLabel("Select the installation directory"));
      vBox.add(hBox);
      getContentPane().add("Center", vBox);
      pack();
      setLocationRelativeTo(null);
      setVisible(true);
    }

    public void next() {
      dispose();
      installDir = new File(tf.getText());
      tryInstall();
    }
  }
}
