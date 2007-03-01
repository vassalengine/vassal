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
package VASSAL.launch.install;

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

/**
 * @author rkinney
 */
public class InstallJnlpScreen extends InstallProgressScreen implements Constants {
  private List resources = new ArrayList();
  private File installFile;
  private String jnlpURL;
  private File installDir;
  private File installLibDir;
  private String heapSize;

  protected void tryInstall(final InstallWizard wizard) throws IOException {
    installDir = new File((String) wizard.get(INSTALL_DIR));
    jnlpURL = (String) wizard.get(JNLP_URL);
    heapSize = (String) wizard.get(Constants.HEAP_SIZE);
    doInstall();
    wizard.getDialog().setScreen(new SuccessScreen("<html>Installation successful.<br>To get started, double-click on " + installFile + "</html>"));
  }

  public void doInstall() throws IOException {
    checkParameters();
    Document doc = getJNLPDoc(new URL(jnlpURL));
    modifyDocument(doc);
    downloadFiles(doc, installFile);
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
      String path = getFileName(resource);
      el.removeAttribute("version");
      el.setAttribute("href", "lib/" + path);
      resources.add(resource);
    }
    NodeList icons = doc.getElementsByTagName("icon");
    for (int i = 0, n = icons.getLength(); i < n; ++i) {
      Element el = (Element) icons.item(i);
      String href = el.getAttribute("href");
      URL resource = base == null ? new URL(href) : new URL(base, href);
      el.setAttribute("href", "lib/" + href);
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
      el.setAttribute("href", "lib/" + path);
      writeXmlDocument(extensionDoc, new File(installLibDir, path));
    }
  }

  public String getFileName(URL url) {
    String path = url.getPath();
    path = path.substring(path.lastIndexOf('/') + 1);
    return path;
  }

  private void downloadResource(URL resource) throws IOException {
    byte[] buffer = new byte[100000];
    int readCount = 0;
    String path = getFileName(resource);
    File local = new File(new File(installDir, "lib"), path);
    FileOutputStream out = new FileOutputStream(local);
    InputStream in = resource.openStream();
    while ((readCount = in.read(buffer)) > 0) {
      out.write(buffer, 0, readCount);
    }
    out.close();
  }

  protected void downloadFiles(Document doc, File file) throws IOException {
    writeXmlDocument(doc, file);
    for (Iterator it = resources.iterator(); it.hasNext();) {
      URL resource = (URL) it.next();
      setStatus("Downloading files: " + getFileName(resource));
      downloadResource(resource);
    }
  }

  protected void writeXmlDocument(Document doc, File file) throws IOException {
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
    extractResources(doc);
    NodeList l = doc.getElementsByTagName("*");
    for (int i = 0, n = l.getLength(); i < n; ++i) {
      Node node = l.item(i);
      Node child = node.getFirstChild();
      while (child != null) {
        Node next = child.getNextSibling();
        if (child.getNodeType() == Node.TEXT_NODE && child.getNodeValue() != null && child.getNodeValue().trim().length() == 0) {
          node.removeChild(child);
        }
        child = next;
      }
    }
    l = doc.getElementsByTagName("j2se");
    if (l.getLength() == 1) {
      Element el = (Element) l.item(0);
      el.setAttribute("max-heap-size", heapSize);
    }
    doc.getDocumentElement().setAttribute("codebase", installDir.toURL().toString());
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
    installLibDir = new File(installDir, "lib");
    if (!installLibDir.exists() && !installLibDir.mkdir()) {
      throw new IOException("Unable to create " + installLibDir);
    }
    if (jnlpURL == null) {
      throw new IOException("No version specified");
    }
    String file = new URL(jnlpURL).getPath();
    file = file.substring(file.lastIndexOf('/') + 1);
    installFile = new File(installDir, file);
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
