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
package VASSAL.preferences;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerLayout;
import VASSAL.i18n.Resources;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.SplashScreen;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.swing.SwingUtils;
import net.miginfocom.swing.MigLayout;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PrefsEditor {
  private JDialog
    dialog;
  private final List<Configurer> options = new ArrayList<>();
  private final List<Configurer> extras = new ArrayList<>();
  private boolean iterating = false;
  private final Map<Configurer, Object> savedValues = new HashMap<>();
  private final List<Prefs> prefs = new ArrayList<>();
  private final JTabbedPane optionsTab = new JTabbedPane();
  private JDialog setupDialog;
  private Action editAction;
  private JButton ok;
  private JButton cancel;

  public void initDialog(Frame parent) {
    if (dialog == null) {
      dialog = new JDialog(parent, true);
      dialog.setTitle(Resources.getString("Prefs.preferences")); //$NON-NLS-1$
      dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

      // Handle window closing correctly.
      dialog.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent we) {
          cancel();
        }
      });

      // Help button looks up help in Preferences.html, by tab-name
      final JButton help = new JButton(Resources.getString(Resources.HELP));
      help.addActionListener(e -> {
        final int index = optionsTab.getSelectedIndex();
        final String tabName = (index >= 0) ? optionsTab.getTitleAt(index) : "top"; //NON-NLS
        final HelpFile helpFile = HelpFile.getReferenceManualPage("Preferences.html", tabName); //NON-NLS
        BrowserSupport.openURL(helpFile.getContents().toString());
      });

      ok = new JButton(Resources.getString(Resources.OK));
      ok.addActionListener(e -> save());

      cancel = new JButton(Resources.getString(Resources.CANCEL));
      cancel.addActionListener(e -> cancel());

      dialog.setLayout(new MigLayout("insets dialog", "[push,fill]"));

      final JPanel contentPanel = new JPanel(new MigLayout("insets dialog", "[push,fill]"));
      contentPanel.add(optionsTab, "push, grow, wrap unrelated"); //NON-NLS
      final JScrollPane scrollPane = new JScrollPane(contentPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

      final JPanel scrollPanel = new JPanel(new BorderLayout());
      scrollPanel.add(scrollPane, BorderLayout.CENTER);
      dialog.add(scrollPanel, "grow, wrap");

      final JPanel buttonPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS
      buttonPanel.add(ok, "tag ok, sg 1"); //NON-NLS
      buttonPanel.add(cancel, "tag cancel, sg 1"); //NON-NLS
      buttonPanel.add(help, "tag help, split, sg 1"); //NON-NLS
      dialog.add(buttonPanel, "grow");

      // Default actions for Enter/ESC
      dialog.getRootPane().setDefaultButton(ok);
      dialog.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
        KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "Cancel"); //$NON-NLS-1$
      dialog.getRootPane().getActionMap().put("Cancel", new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
          cancel.doClick();
        }
      });
    }
  }

  public JDialog getDialog() {
    return dialog;
  }

  public void addPrefs(Prefs p) {
    prefs.add(p);
  }

  private JButton okButton;

  public void addOption(String category, Configurer c, String prompt) {
    if (prompt != null) {
      if (setupDialog == null) {
        setupDialog = new JDialog(GameModule.getGameModule().getPlayerWindow(), true);
        setupDialog.setTitle(Resources.getString("Prefs.initial_setup")); //$NON-NLS-1$
        setupDialog.setLayout(new BoxLayout(setupDialog.getContentPane(), BoxLayout.Y_AXIS));
        setupDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        setupDialog.addComponentListener(new ComponentAdapter() {
          @Override
          public void componentShown(ComponentEvent e) {
            SplashScreen.sendAllToBack();
          }
        });
      }
      JPanel p = new JPanel();
      p.add(new JLabel(prompt));
      setupDialog.add(p);
      setupDialog.add(c.getControls());
      final JButton b = okButton = new JButton(Resources.getString(Resources.OK));
      b.addActionListener(evt -> setupDialog.setVisible(false));
      p = new JPanel();
      p.add(b);
      setupDialog.add(p);

      // Default actions for Enter/ESC
      setupDialog.getRootPane().setDefaultButton(okButton);
      setupDialog.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
        KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "Cancel"); //$NON-NLS-1$
      setupDialog.getRootPane().getActionMap().put("Cancel", new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
          okButton.doClick();
        }
      });

      setupDialog.pack();
      final Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
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

    JPanel pan;

    final int i = optionsTab.indexOfTab(category);
    if (i == -1) { // No match
      pan = new JPanel();
      pan.setLayout(new MigLayout("ins panel," + ConfigurerLayout.STANDARD_GAPY, "[right][fill,grow]")); // NON-NLS
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

    final String name = c.getName();
    final JLabel label = new JLabel(name);
    c.setLabelVisibile(false);
    label.setLabelFor(c.getControls());
    pan.add(label);
    pan.add(c.getControls(), "grow,wrap,aligny center"); // NON-NLS
  }

  private synchronized void storeValues() {
    savedValues.clear();
    for (final Configurer c : options) {
      c.setFrozen(true);
      if (c.getValue() != null) {
        savedValues.put(c, c.getValue());
      }
    }
  }

  protected synchronized void cancel() {
    for (final Configurer c : options) {
      final Object o = savedValues.get(c);
      if (o != null) {
        c.setValue(o);
      }
      c.setFrozen(false);
    }
    dialog.setVisible(false);
    for (final Prefs p : prefs) {
      p.setDisableAutoWrite(false); //BR// Turn auto-write back on, if this was globalPrefs
    }
  }

  protected synchronized void save() {
    iterating = true;
    for (final Configurer c : options) {
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

    for (final Prefs p : prefs) {
      p.setDisableAutoWrite(false); //BR// Turn auto-write back on, if this was globalPrefs
    }
  }

  public Action getEditAction() {
    if (editAction == null) {
      editAction = new AbstractAction(
          Resources.getString("Prefs.edit_preferences")) { //$NON-NLS-1$
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          for (final Prefs p : prefs) {
            p.setDisableAutoWrite(true); //BR// Turn off auto-write while we're editing globalPrefs (we will write the whole thing at the end)
          }
          storeValues();
          dialog.pack();
          final Dimension d = SwingUtils.getScreenSize();
          dialog.setLocation(d.width / 2 - dialog.getWidth() / 2, 0);
          SwingUtils.ensureOnScreen(dialog);
          dialog.setVisible(true);
        }
      };
      // FIXME: setting mnemonic from first letter could cause collisions in some languages
      editAction.putValue(Action.MNEMONIC_KEY, (int) Resources.getString("Prefs.edit_preferences").charAt(0));
    }
    return editAction;
  }

  public void write() {
    for (final Prefs p : prefs) {
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
