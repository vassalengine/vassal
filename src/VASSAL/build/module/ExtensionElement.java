/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.build.module;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.tools.ComponentPathBuilder;

/**
 * An element of a {@link ModuleExtension} that extends an
 * individual {@link VASSAL.build.Buildable} component of the
 * {@link VASSAL.build.GameModule}.
 */
public class ExtensionElement implements Buildable {
  /**
   * An identifier for the component to be extended
   */
  public static final String TARGET = "target"; //$NON-NLS-1$
  private Buildable extension;
  private Configurable[] targetPath;

  public ExtensionElement() {
  }

  public ExtensionElement(Buildable extension, Configurable[] targetPath) {
    this.extension = extension;
    this.targetPath = targetPath;
  }

  public void add(Buildable child) {
    extension = child;
  }

  public void build(Element e) {
    try {
      targetPath =
        ComponentPathBuilder.getInstance().getPath(e.getAttribute(TARGET));
    }
    catch (ComponentPathBuilder.PathFormatException e1) {
      throw new ExtensionsLoader.LoadExtensionException(e1.getMessage());
    }

    // find and build first child which is an element
    for (Node n = e.getFirstChild(); n != null; n = n.getNextSibling()) {
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        extension = Builder.create((Element) n);
        break;
      }
    }
  }

  public Buildable getExtension() {
    return extension;
  }

  public Configurable[] getTargetPath() {
    return targetPath;
  }

  public Element getBuildElement(Document doc) {
    final Element el = doc.createElement(getClass().getName());
    el.setAttribute(TARGET,
                    ComponentPathBuilder.getInstance().getId(targetPath));
    el.appendChild(extension.getBuildElement(doc));
    return el;
  }

  public void addTo(Buildable parent) {
    final Configurable target = targetPath.length == 0 ?
      GameModule.getGameModule() : targetPath[targetPath.length - 1];
    extension.addTo(target);
    target.add(extension);
  }
}
