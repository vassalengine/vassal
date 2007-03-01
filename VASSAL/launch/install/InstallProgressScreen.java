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

import java.awt.Component;
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
import javax.swing.JLabel;
import javax.swing.JProgressBar;
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
public abstract class InstallProgressScreen implements Screen {

  protected Box controls;
  protected JLabel status;
  protected JProgressBar progress;
  public InstallProgressScreen() {
    super();
    controls = Box.createVerticalBox();
    status = new JLabel("Downloading files");
    progress = new JProgressBar();
    progress.setIndeterminate(true);
    controls.add(status);
    controls.add(progress);
  }
  public void start(final InstallWizard wizard) {
    new Thread() {
      public void run() {
        try {
          tryInstall(wizard);
        }
        catch (IOException e) {
          e.printStackTrace();
          wizard.getDialog().setScreen(new FailureScreen(e));
        }
        catch (RuntimeException e) {
          e.printStackTrace();
          wizard.getDialog().setScreen(new FailureScreen(e));
        }
      }
    }.start();    
  }
  public Component getControls() {
    return controls;
  }

  public void next(InstallWizard wizard) {
  }
  public void setStatus(String msg) {
    status.setText(msg);
  }
  protected abstract void tryInstall(final InstallWizard wizard) throws IOException;
  
}