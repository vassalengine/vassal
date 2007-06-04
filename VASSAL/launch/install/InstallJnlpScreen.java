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

import VASSAL.i18n.Resources;

/**
 * @author rkinney
 */
public class InstallJnlpScreen extends InstallProgressScreen
                               implements Constants {
  protected List<URL> resources = new ArrayList<URL>();
  protected File installFile;
  protected String jnlpURL;
  protected String internalResources;
  protected File installDir;
  protected File installLibDir;
  protected String heapSize;

  protected void tryInstall(final InstallWizard wizard) throws IOException {
    prepareInstall(wizard);
    doInstall();
    wizard.getDialog().setScreen(new SuccessScreen(
        "<html>" + Resources.getString("Install.install_successful") + "<br>" +  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                   Resources.getString("Install.to_get_started", installFile.toString()) + "</html>")); //$NON-NLS-1$ //$NON-NLS-2$
  }

  protected void prepareInstall(final InstallWizard wizard) throws IOException {
    if (wizard.get(INSTALL_DIR) == null) {
      throw new IOException(Resources.getString("Install.error_install_dir")); //$NON-NLS-1$
    }
    installDir = new File(wizard.get(INSTALL_DIR));
    if (installDir.exists() && !installDir.isDirectory()) {
      throw new IOException(Resources.getString("Install.error_not_a_directory", installDir.toString())); //$NON-NLS-1$
    }
    if (!installDir.exists() && !installDir.mkdir()) {
      throw new IOException(Resources.getString("Install.error_unable_to_create", installDir.toString())); //$NON-NLS-1$
    }
    installLibDir = new File(installDir, "lib"); //$NON-NLS-1$
    if (!installLibDir.exists() && !installLibDir.mkdir()) {
      throw new IOException(Resources.getString("Install.error_unable_to_create", installLibDir.toString())); //$NON-NLS-1$
    }
    jnlpURL = wizard.get(JNLP_URL);
    if (jnlpURL == null) {
      throw new IOException(Resources.getString("Install.error_no_version")); //$NON-NLS-1$
    }
    String file = new URL(jnlpURL).getPath();
    file = file.substring(file.lastIndexOf('/') + 1);
    installFile = new File(installDir, file);
    heapSize = wizard.get(Constants.HEAP_SIZE);
    internalResources = wizard.get(INTERNAL_RESOURCES);
  }

  public void doInstall() throws IOException {
    Document doc = getJNLPDoc(new URL(jnlpURL));
    modifyDocument(doc);
    if (internalResources != null) {
      String[] resource = internalResources.split(",");
      for (int i = 0; i < resource.length; i++) {
        resources.add(getClass().getResource("/"+resource[i]));
      }
    }
    downloadFiles(doc, installFile);
  }

  protected void extractResources(Document doc) throws IOException {
    String codebase = doc.getDocumentElement().getAttribute("codebase"); //$NON-NLS-1$
    URL base = null;
    if (codebase != null) {
      if (!codebase.endsWith("/")) { //$NON-NLS-1$
        codebase += "/"; //$NON-NLS-1$
      }
      base = new URL(codebase);
    }
    NodeList jars = doc.getElementsByTagName("jar"); //$NON-NLS-1$
    for (int i = 0, n = jars.getLength(); i < n; ++i) {
      Element el = (Element) jars.item(i);
      String version = el.getAttribute("version"); //$NON-NLS-1$
      String href = el.getAttribute("href"); //$NON-NLS-1$
      String url = version == null || version.length() == 0 ? href : href + "?version-id=" + version; //$NON-NLS-1$
      URL resource = base == null ? new URL(url) : new URL(base, url);
      String path = getFileName(resource);
      el.removeAttribute("version"); //$NON-NLS-1$
      el.setAttribute("href", "lib/" + path); //$NON-NLS-1$ //$NON-NLS-2$
      resources.add(resource);
    }
    NodeList icons = doc.getElementsByTagName("icon"); //$NON-NLS-1$
    for (int i = 0, n = icons.getLength(); i < n; ++i) {
      Element el = (Element) icons.item(i);
      String href = el.getAttribute("href"); //$NON-NLS-1$
      URL resource = base == null ? new URL(href) : new URL(base, href);
      el.setAttribute("href", "lib/" + href); //$NON-NLS-1$ //$NON-NLS-2$
      resources.add(resource);
    }
    NodeList l = doc.getElementsByTagName("extension"); //$NON-NLS-1$
    for (int i = 0, n = l.getLength(); i < n; ++i) {
      Element el = (Element) l.item(i);
      String href = el.getAttribute("href"); //$NON-NLS-1$
      URL url = base == null ? new URL(href) : new URL(base, href);
      Document extensionDoc = getJNLPDoc(url);
      modifyDocument(extensionDoc);
      String path = getFileName(url);
      el.setAttribute("href", "lib/" + path); //$NON-NLS-1$ //$NON-NLS-2$
      writeXmlDocument(extensionDoc, new File(installLibDir, path));
    }
  }

  public String getFileName(URL url) {
    String path = url.getPath();
    path = path.substring(path.lastIndexOf('/') + 1);
    return path;
  }

  protected void downloadResource(URL resource) throws IOException {
    byte[] buffer = new byte[100000];
    int readCount = 0;
    String path = getFileName(resource);
    File local = new File(new File(installDir, "lib"), path); //$NON-NLS-1$
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
      setStatus(Resources.getString("Install.downloading", getFileName(resource))); //$NON-NLS-1$
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
      xformer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
      xformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2"); //$NON-NLS-1$ //$NON-NLS-2$
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
    NodeList l = doc.getElementsByTagName("*"); //$NON-NLS-1$
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
    l = doc.getElementsByTagName("j2se"); //$NON-NLS-1$
    if (l.getLength() == 1) {
      Element el = (Element) l.item(0);
      el.setAttribute("max-heap-size", heapSize); //$NON-NLS-1$
    }
    doc.getDocumentElement().setAttribute("codebase", //$NON-NLS-1$
        installDir.toURI().toURL().toString());
    doc.getDocumentElement().setAttribute("href", installFile.getName());
  }

  public Document getJNLPDoc(URL url) throws IOException {
    Document d;
    try {
      DocumentBuilderFactory f = javax.xml.parsers.DocumentBuilderFactory.newInstance();
      f.setIgnoringElementContentWhitespace(true);
      d = f.newDocumentBuilder().parse(url.openStream());
    }
    catch (SAXException e) {
      throw new IOException("SAXException:  " + e.getMessage()); //$NON-NLS-1$
    }
    catch (ParserConfigurationException e) {
      throw new IOException("ParserConfigurationException:  " + e.getMessage()); //$NON-NLS-1$
    }
    catch (FactoryConfigurationError e) {
      throw new IOException("FactoryConfigurationError:  " + e.getMessage()); //$NON-NLS-1$
    }
    return d;
  }
}
