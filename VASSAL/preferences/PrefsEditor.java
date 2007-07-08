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
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.SplashScreen;

public class PrefsEditor {
  private JDialog dialog;
  private List<Configurer> options = new ArrayList<Configurer>();
  private Map<Configurer,Object> savedValues;
  private List<Prefs> prefs;
  private JButton save, cancel;
  private JTabbedPane optionsTab;
  private JDialog setupDialog;
  private ArchiveWriter archive;
  private Action editAction;
  private final JPanel buttonPanel = new JPanel();

  public PrefsEditor(ArchiveWriter archive) {

    savedValues = new HashMap<Configurer,Object>();
    this.archive = archive;

    prefs = new ArrayList<Prefs>();
    optionsTab = new JTabbedPane();
  }

  public void initDialog(Frame parent) {
    if (dialog == null) {
      dialog = new JDialog(parent, true);
      dialog.setTitle(Resources.getString("Prefs.preferences")); //$NON-NLS-1$
      dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

      //Handle window closing correctly.
      dialog.addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent we) {
          cancel();
        }
      });

      save = new JButton(Resources.getString(Resources.OK));
      save.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          save();
        }
      });
      cancel = new JButton(Resources.getString(Resources.CANCEL));
      cancel.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          cancel();
        }
      });

      buttonPanel.add(save);
      buttonPanel.add(cancel);
      
      dialog.getContentPane().setLayout
          (new BoxLayout(dialog.getContentPane(), BoxLayout.Y_AXIS));
      dialog.getContentPane().add(optionsTab);
      dialog.getContentPane().add(buttonPanel);
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
        setupDialog.getContentPane().setLayout
            (new BoxLayout(setupDialog.getContentPane(), BoxLayout.Y_AXIS));
        setupDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        setupDialog.addComponentListener(new ComponentAdapter() {
          public void componentShown(ComponentEvent e) {
            SplashScreen.sendAllToBack();
          }
        });
      }
      JPanel p = new JPanel();
      p.add(new JLabel(prompt));
      setupDialog.getContentPane().add(p);
      setupDialog.getContentPane().add(c.getControls());
      JButton b = new JButton(Resources.getString(Resources.OK));
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          setupDialog.setVisible(false);
        }
      });
      p = new JPanel();
      p.add(b);
      setupDialog.getContentPane().add(p);
      setupDialog.pack();
      Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      setupDialog.setLocation(d.width / 2 - setupDialog.getSize().width / 2,
                              d.height / 2 - setupDialog.getSize().height / 2);
      setupDialog.setVisible(true);
      setupDialog.getContentPane().removeAll();
    }
    addOption(category, c);
  }

  public void addOption(String category, Configurer c) {
    if (category == null) {
      category = Resources.getString("Prefs.general_tab"); //$NON-NLS-1$
    }
    JPanel pan = null;
    int i = 0;
    for (i = 0; i < optionsTab.getTabCount(); ++i) {
      if (category.equals(optionsTab.getTitleAt(i))) {
        pan = (JPanel) optionsTab.getComponentAt(i);
        break;
      }
    }
    if (i >= optionsTab.getTabCount()) { // No match
      pan = new JPanel();
      pan.setLayout(new BoxLayout(pan, BoxLayout.Y_AXIS));
      optionsTab.addTab(category, pan);
    }
    options.add(c);
    Box b = Box.createHorizontalBox();
    b.add(c.getControls());
    b.add(Box.createHorizontalGlue());
    pan.add(b);
  }

  private void storeValues() {
    savedValues.clear();
    for (Configurer c : options) {
      c.setFrozen(true);
      if (c.getValue() != null) {
        savedValues.put(c, c.getValue());
      }
    }
  }

  public ArchiveWriter getArchive() {
    return archive;
  }

  protected void cancel() {
    for (Configurer c : options) {
      c.setValue(savedValues.get(c));
      c.setFrozen(false);
    }
    dialog.setVisible(false);
  }

  protected void save() {
    for (Configurer c : options) {
      c.fireUpdate();
      c.setFrozen(false);
    }
    try {
      write();
    }
    catch (IOException e) {
      JOptionPane.showMessageDialog(dialog.getOwner(), Resources.getString("Prefs.unable_to_save"), Resources.getString("Prefs.save_error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
    }
    dialog.setVisible(false);
  }

  public Action getEditAction() {
    if (editAction == null) {
      editAction = new AbstractAction(Resources.getString("Prefs.edit_preferences")) { //$NON-NLS-1$
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          storeValues();
          dialog.pack();
          Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
          dialog.setLocation(d.width / 2 - dialog.getWidth() / 2, 0);
          dialog.setVisible(true);
        }
      };
      editAction.putValue(Action.MNEMONIC_KEY, new Integer('P'));
    }
    return editAction;
  }

  public void write() throws IOException {
    for (Prefs p : prefs) {
      p.save();
    }
    archive.write();
  }
}

