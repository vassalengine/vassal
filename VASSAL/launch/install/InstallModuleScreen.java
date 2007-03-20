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
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import VASSAL.i18n.Resources;

/**
 * @author rkinney
 */
public class InstallModuleScreen extends InstallJnlpScreen {
  protected String moduleFile;
  protected String title;

  protected void modifyDocument(Document doc) throws IOException {
    super.modifyDocument(doc);
    NodeList l = doc.getElementsByTagName("application-desc"); //$NON-NLS-1$
    if (l.getLength() > 0) {
      Node desc = l.item(0);
      ArrayList argList = new ArrayList();
      NodeList args = doc.getElementsByTagName("argument"); //$NON-NLS-1$
      for (int i = 0; i < args.getLength(); i++) {
        Node arg = args.item(i);
        if (desc.equals(arg.getParentNode())) {
          argList.add(arg);
        }
      }
      for (Iterator it = argList.iterator(); it.hasNext();) {
        Node arg = (Node) it.next();
        desc.removeChild(arg);
      }
      Element arg = doc.createElement("argument"); //$NON-NLS-1$
      File module = new File(new File(installDir,"lib"), moduleFile);
      arg.appendChild(doc.createTextNode(module.getPath())); //$NON-NLS-1$
      desc.appendChild(arg);
    }
    l = doc.getElementsByTagName("title");
    if (l.getLength() > 0) {
      Node title = l.item(0);
      while (title.getLastChild() != null) {
        title.removeChild(title.getLastChild());
      }
      title.appendChild(doc.createTextNode(this.title));
    }
  }

  protected void prepareInstall(InstallWizard wizard) throws IOException {
    moduleFile = wizard.get(MODULE_FILE);
    if (moduleFile == null) {
      throw new IOException(Resources.getString("Install.module_not_specified")); //$NON-NLS-1$
    }
    title = wizard.get(JNLP_TITLE);
    super.prepareInstall(wizard);
    int index = moduleFile.lastIndexOf('.');
    String moduleFileRoot = index < 0 ? moduleFile : moduleFile.substring(0,index);
    installFile = new File(installFile.getParentFile(),moduleFileRoot+".jnlp");
  }
}
