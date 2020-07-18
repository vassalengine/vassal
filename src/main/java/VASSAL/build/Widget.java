/*
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
package VASSAL.build;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Localization;

/**
 * A general-purpose configurable GUI container
 * Widgets are {@link Configurable} objects that represent
 * AWT components.  Adding a Widget to another Widget during
 * a {@link Buildable#build} operation will add the corresponding
 * AWT component of the child to the component of the parent
 */
public abstract class Widget extends AbstractConfigurable {
  public static final String NAME = "entryName";
  public static final String WIDTH="width";
  public static final String HEIGHT="height";
  protected Element buildElement;

  protected Widget parent;

  protected Widget() {
  }

  /**
   * For memory efficiency reasons, a Widget is initialized lazily.
   * This method only stores the element from which the build the Widget.
   * The Widget is built from the stored element by invoking {@link #rebuild}.
   * Subclasses should invoke {@link #rebuild} before invoking {@link #getComponent}
   */
  @Override
  public void build(Element el) {
    buildElement = el;
    if (el != null) {
      NamedNodeMap n = el.getAttributes();
      for (int i = 0; i < n.getLength(); ++i) {
        Attr att = (Attr) n.item(i);
        setAttribute(att.getName(), att.getValue());
        Localization.getInstance().saveTranslatableAttribute(this, att.getName(), att.getValue());
      }
    }
  }

  /**
   * Perform the build of this {@link Buildable} component using the element
   * stored from when the the {@link #build} method was invoked
   */
  protected void rebuild() {
    if (buildElement != null) {
      Builder.build(buildElement, this);
      buildElement = null;
    }
  }

  @Override
  public Element getBuildElement(Document doc) {
    rebuild();
    return super.getBuildElement(doc);
  }

  /**
   * The allowable Configurable components of a Widget are the same
   * as its parent
   */
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return parent.getAllowableConfigureComponents();
  }

  @Override
  public void addTo(Buildable b) {
    parent = (Widget) b;
  }

  @Override
  public void removeFrom(Buildable b) {
  }

  public Widget getParent() {
    return parent;
  }

  @Override
  public Configurable[] getConfigureComponents() {
    rebuild();
    return super.getConfigureComponents();
  }

  /**
   * @return the Component for this widget.  For efficiency, the
   * Component may be initialized lazily instead of being created
   * in the {@link Buildable#build} method
   */
  public abstract java.awt.Component getComponent();

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  public static class MyCellRenderer extends javax.swing.DefaultListCellRenderer {
    private static final long serialVersionUID = 1L;

    @Override
    public java.awt.Component getListCellRendererComponent(
      javax.swing.JList list,
      Object value,
      int index,
      boolean isSelected,
      boolean cellHasFocus) {

      super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
      if (value instanceof Configurable)
        setText(((Configurable) value).getConfigureName());
      return this;
    }
  }
}
