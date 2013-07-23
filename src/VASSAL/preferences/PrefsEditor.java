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
package VASSAL.preferences;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.SplashScreen;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.io.ZipArchive;

public class PrefsEditor {
  private JDialog dialog;
  private List<Configurer> options = new ArrayList<Configurer>();
  private List<Configurer> extras = new ArrayList<Configurer>();
  private boolean iterating = false;
  private Map<Configurer,Object> savedValues = new HashMap<Configurer,Object>();
  private List<Prefs> prefs = new ArrayList<Prefs>();
  private JTabbedPane optionsTab = new JTabbedPane();
  private JDialog setupDialog;
  private File pfile;
  private Action editAction;

  public PrefsEditor() {}

  public void initDialog(Frame parent) {
    if (dialog == null) {
      dialog = new JDialog(parent, true);
      dialog.setTitle(Resources.getString("Prefs.preferences")); //$NON-NLS-1$
      dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

      // Handle window closing correctly.
      dialog.addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent we) {
          cancel();
        }
      });

      final JButton ok = new JButton(Resources.getString(Resources.OK));
      ok.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          save();
        }
      });

      final JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
      cancel.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          cancel();
        }
      });

      dialog.setLayout(new MigLayout("insets dialog"));
      dialog.add(optionsTab, "push, grow, wrap unrelated");
      dialog.add(ok, "tag ok, split");
      dialog.add(cancel, "tag cancel");
    }
  }

  public JDialog getDialog() {
    return dialog;
  }

  public void addPrefs(Prefs p) {
    prefs.add(p);
  }

  public void addOption(String category, Configurer c, String prompt) {
    if (prompt != null) {
      if (setupDialog == null) {
        setupDialog = new JDialog((Frame) null, true);
        setupDialog.setTitle(Resources.getString("Prefs.initial_setup")); //$NON-NLS-1$
        setupDialog.setLayout(new BoxLayout(setupDialog.getContentPane(), BoxLayout.Y_AXIS));
        setupDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        setupDialog.addComponentListener(new ComponentAdapter() {
          public void componentShown(ComponentEvent e) {
            SplashScreen.sendAllToBack();
          }
        });
      }
      JPanel p = new JPanel();
      p.add(new JLabel(prompt));
      setupDialog.add(p);
      setupDialog.add(c.getControls());
      JButton b = new JButton(Resources.getString(Resources.OK));
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          setupDialog.setVisible(false);
        }
      });
      p = new JPanel();
      p.add(b);
      setupDialog.add(p);
      setupDialog.pack();
      Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      setupDialog.setLocation(
        d.width / 2 - setupDialog.getSize().width / 2,
        d.height / 2 - setupDialog.getSize().height / 2
      );
      setupDialog.setVisible(true);
      setupDialog.removeAll();
    }
    addOption(category, c);
  }

  public synchronized void addOption(String category, Configurer c) {
    if (category == null) {
      category = Resources.getString("Prefs.general_tab"); //$NON-NLS-1$
    }

    JPanel pan = null;

    int i = optionsTab.indexOfTab(category);
    if (i == -1) { // No match
      pan = new JPanel();
      pan.setLayout(new BoxLayout(pan, BoxLayout.Y_AXIS));
      optionsTab.addTab(category, pan);
    }
    else {
      pan = (JPanel) optionsTab.getComponentAt(i);
    }

    if (iterating) {
      extras.add(c);
    }
    else {
      options.add(c);
    }

    final Box b = Box.createHorizontalBox();
    b.add(c.getControls());
    b.add(Box.createHorizontalGlue());
    pan.add(b);
  }

  private synchronized void storeValues() {
    savedValues.clear();
    for (Configurer c : options) {
      c.setFrozen(true);
      if (c.getValue() != null) {
        savedValues.put(c, c.getValue());
      }
    }
  }

  protected synchronized void cancel() {
    for (Configurer c : options) {
      c.setValue(savedValues.get(c));
      c.setFrozen(false);
    }
    dialog.setVisible(false);
  }

  protected synchronized void save() {
    iterating = true;
    for (Configurer c : options) {
      if ((savedValues.get(c) == null && c.getValue() != null) || (savedValues.get(c) != null && !savedValues.get(c).equals(c.getValue()))) {
        c.fireUpdate();
      }
      c.setFrozen(false);
    }

    iterating = false;
    options.addAll(extras);
    extras.clear();

    write();

    dialog.setVisible(false);
  }

  public Action getEditAction() {
    if (editAction == null) {
      editAction = new AbstractAction(
          Resources.getString("Prefs.edit_preferences")) { //$NON-NLS-1$
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          storeValues();
          dialog.pack();
          final Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
          dialog.setLocation(d.width / 2 - dialog.getWidth() / 2, 0);
          dialog.setVisible(true);
        }
      };
      // FIMXE: setting nmemonic from first letter could cause collisions in
      // some languages
      editAction.putValue(Action.MNEMONIC_KEY, (int) Resources.getString("Prefs.edit_preferences").charAt(0));
    }
    return editAction;
  }

  public void write() {
    for (Prefs p : prefs) {
      try {
        p.save();
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, p.getFile());
      }
    }
  }

  public void close() {
    write();
  }
}
