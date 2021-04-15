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
package VASSAL.configure;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.widget.PieceSlot;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

import VASSAL.tools.swing.SwingUtils;
import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import net.miginfocom.swing.MigLayout;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

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
      final Node nextChild = child.getNextSibling();
      if (Node.ELEMENT_NODE == child.getNodeType()) {
        // Cull Buildables from the state.
        try {
          final Class<?> c = GameModule.getGameModule().getDataArchive().loadClass(((Element)child).getTagName());
          if (Buildable.class.isAssignableFrom(c)) {
            originalState.removeChild(child);
          }
        }
        catch (ClassNotFoundException e) {
          // This element doesn't correspond to a class. Skip it.
        }
        catch (LinkageError e) {
          ErrorDialog.bug(e);
        }
      }
      child = nextChild;
    }

    setLayout(new MigLayout("ins panel,wrap 1", "[grow,fill]", "[align top,grow][]")); // NON-NLS
    configurer = target.getConfigurer();
    target.addPropertyChangeListener(evt -> {
      if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
        setTitle((String) evt.getNewValue());
      }
    });

    setTitle(ConfigureTree.getConfigureName(target));

    final JPanel buttonBox = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS
    final JButton okButton = new JButton(Resources.getString("General.ok"));
    okButton.addActionListener(e -> save());
    buttonBox.add(okButton, "sg,tag ok"); // NON-NLS

    final JButton cancelButton = new JButton(Resources.getString("General.cancel"));
    cancelButton.addActionListener(e -> cancel());
    buttonBox.add(cancelButton, "sg,tag cancel"); // NON-NLS

    if (target.getHelpFile() != null) {
      final Action helpAction = new ShowHelpAction(target.getHelpFile().getContents(), null);
      final JButton helpButton = new JButton(helpAction);
      buttonBox.add(helpButton, "sg,tag help"); // NON-NLS
    }

    // The PieceDefiner handles its own scrolling internally, don't add another set of scrollbars
    if (target instanceof PieceSlot || target instanceof PrototypeDefinition) {
      add(configurer.getControls(), "grow"); // NON-NLS
    }
    else {
      final JPanel scrollPanel = new JPanel(new MigLayout("ins 0,wrap 1", "[grow,fill]")); // NON-NLS
      final JScrollPane scroll = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      scrollPanel.add(configurer.getControls());
      add(scroll);
    }

    add(buttonBox, "growy 0"); // NON-NLS

    pack();
    setLocationRelativeTo(getParent());
    SwingUtils.ensureOnScreen(this);
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent we) {
        cancel();
      }
    });
  }

  public void cancel() {
    if (target instanceof GameModule) { // Modules we don't want to do the full scary rebuild, just put the text fields back.
      target.setAttribute(GameModule.MODULE_NAME, originalState.getAttribute(GameModule.MODULE_NAME));
      target.setAttribute(GameModule.MODULE_VERSION, originalState.getAttribute(GameModule.MODULE_VERSION));
      target.setAttribute(GameModule.DESCRIPTION, originalState.getAttribute(GameModule.DESCRIPTION));
    }
    else if (target instanceof Map) {
      //BR// Keeps us from wiping the whole map contents (running in the player right now) just because user hit cancel.
      //BR// Possibly this would be better for all/most AbstractConfigurable items. But probably a matter for further research & experiment.
      for (final String propName : ((Map)target).getAttributeNames()) {
        target.setAttribute(propName, originalState.getAttribute(propName));
      }
    }
    else {
      target.build(originalState);
    }
    dispose();
  }

  public void save() {
    configurer.getValue();
    dispose();
  }
}
