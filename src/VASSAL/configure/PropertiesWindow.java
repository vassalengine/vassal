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
package VASSAL.configure;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.tools.ErrorDialog;

/**
 * Window for editing properties of a {@link Configurable} object
 */
public class PropertiesWindow extends JDialog {
  private static final long serialVersionUID = 1L;

  private Configurer configurer;
  private Configurable target;
  private Element originalState;

  public PropertiesWindow(Frame owner, boolean modal, final Configurable target, HelpWindow helpWindow) {
    super(owner, modal);
    initialize(target, helpWindow);
  }

  protected void initialize(final Configurable target, HelpWindow helpWindow) {
    this.target = target;
    originalState = target.getBuildElement(Builder.createNewDocument());
    Node child = originalState.getFirstChild();
    while (child != null) {
      Node nextChild = child.getNextSibling();
      if (Node.ELEMENT_NODE == child.getNodeType()) {
        // Cull Buildables from the state.
        try {
          final Class<?> c = Class.forName(((Element)child).getTagName());
          if (Buildable.class.isAssignableFrom(c)) {
            originalState.removeChild(child);
          }
        }
        catch (ClassNotFoundException e) {
          // This element doesn't correspond to a class. Skip it.
        }
        catch (ExceptionInInitializerError e) {
          ErrorDialog.bug(e);
        }
        catch (LinkageError e) {
          ErrorDialog.bug(e);
        }
      }
      child = nextChild;
    }

    setLayout(new BoxLayout(getContentPane(),BoxLayout.Y_AXIS));
    configurer = target.getConfigurer();
    target.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
          setTitle((String) evt.getNewValue());
        }
      }
    });
    add(configurer.getControls());

    setTitle(ConfigureTree.getConfigureName(target));

    final Box buttonBox = Box.createHorizontalBox();
    final JButton okButton = new JButton("Ok");
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        save();
      }
    });
    buttonBox.add(okButton);

    final JButton cancelButton = new JButton("Cancel");
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        cancel();
      }
    });
    buttonBox.add(cancelButton);

    if (target.getHelpFile() != null) {
      final Action helpAction =
        new ShowHelpAction(target.getHelpFile().getContents(), null);
      final JButton helpButton = new JButton(helpAction);
      buttonBox.add(helpButton);
      pack();
    }

    add(buttonBox);
    pack();
    setLocationRelativeTo(getParent());
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent we) {
         cancel();
      }
    });
  }

  public void cancel() {
    target.build(originalState);
    dispose();
  }

  public void save() {
    configurer.getValue();
    dispose();
  }
}
