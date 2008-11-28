/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
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
package VASSAL.tools;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.TimeUnit;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIManager;

import org.jdesktop.swingworker.SwingWorker;
import org.jdesktop.swingx.JXBusyLabel;
import org.jdesktop.swingx.JXHeader;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.FlowLabel;
import VASSAL.tools.version.VassalVersion;
import VASSAL.tools.version.VersionUtils;

/**
 * @since 3.1.0
 * @author Joel Uckelman
 */ 
public class BugDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  public static void reportABug(Throwable thrown) {
    final Frame frame = GameModule.getGameModule() == null
      ? null : GameModule.getGameModule().getFrame();

    new BugDialog(frame, thrown).setVisible(true);
  }

  private Throwable thrown;
  private String errorLog;

  private JPanel contents;
  private CardLayout deck;

  private JPanel buttons;
  private CardLayout button_deck;

  private JTextArea descriptionArea;
  private JTextField emailField;

/*
    private JButton sendButton;
    private JButton dontSendButton;
    private JButton okButton;
    private JButton cancelButton;
*/

  public BugDialog(Frame owner, Throwable thrown) {
    super(owner, true);

    this.thrown = thrown;
    this.errorLog = BugUtils.getErrorLog();

    //
    // header
    //
    final JXHeader header = new JXHeader(
      Resources.getString("BugDialog.heading"),
      Resources.getString("BugDialog.message"),
      new ImageIcon(BugDialog.class.getResource("/images/bug.png"))
    );

    //
    // dialog
    //
    setTitle(Resources.getString("BugDialog.title"));
    setLocationRelativeTo(owner);
    setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    setResizable(true);

    addWindowListener(new WindowAdapter() {
      public void windowClosed(WindowEvent e) {
        if (checkRequest != null) 
          checkRequest.cancel(true);
       
        if (sendRequest != null) 
          sendRequest.cancel(true);
      }
    });

    add(header, BorderLayout.NORTH);
    add(buildContentsPanel(), BorderLayout.CENTER);
    add(buildButtonPanel(), BorderLayout.SOUTH);
 
    showVersionCheckPanel(); 
    pack();
  }

  private Component buildContentsPanel() {
    deck = new CardLayout();
    contents = new JPanel(deck);
    contents.setBorder(BorderFactory.createEmptyBorder(12,12,0,12));
    
    contents.add(buildVersionCheckPanel(),     "versionCheckPanel");
    contents.add(buildCurrentVersionPanel(),   "currentVersionPanel");
    contents.add(buildSendingBugReportPanel(), "sendingBugReportPanel");
    contents.add(buildOldVersionPanel(),       "oldVersionPanel");
    contents.add(buildConnectionFailedPanel(), "connectionFailedPanel");
    contents.add(buildEmergencySavePanel(),    "emergencySavePanel");

    return contents;
  }

  private Component buildButtonPanel() {
    button_deck = new CardLayout();
    buttons = new JPanel(button_deck); 

    buttons.add(buildVersionCheckButtons(),     "versionCheckButtons");
    buttons.add(buildCurrentVersionButtons(),   "currentVersionButtons");
    buttons.add(buildSendingBugReportButtons(), "sendingBugReportButtons");
    buttons.add(buildOldVersionButtons(),       "oldVersionButtons");
    buttons.add(buildConnectionFailedButtons(), "connectionFailedButtons");
    buttons.add(buildEmergencySaveButtons(),    "emergencySaveButtons");

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);
    panel.setBorder(BorderFactory.createEmptyBorder(17,12,12,12));

// FIXME: use BASELINE_LEADING, BASELINE_TRAILING instead of
// LINE_START and LINE_END when we move to Java 6

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(Box.createHorizontalGlue(), c);

    c.gridx = 1;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,12,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(buttons, c);

/*
    sendButton = new JButton("Send");
//      sendButton.setEnabled(false);

    dontSendButton = new JButton(
      new AbstractAction("Don't Send") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
        }
      }
    );

    okButton = new JButton("Ok");

    cancelButton = new JButton(
      new AbstractAction("Cancel") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      }
    );
*/

/*
    final JPanel panel = new JPanel();
    final GroupLayout layout = new GroupLayout(panel);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);

    layout.setHorizontalGroup(
      layout.createSequentialGroup()
        .add(dontShowAgainCheckBox)
        .add(sendButton)
        .add(dontSendButton));

    layout.setVerticalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(dontShowAgainCheckBox)
        .add(0, 0, Integer.MAX_VALUE)
        .add(sendButton)
        .add(dontSendButton));

    layout.linkSize(
      new Component[]{sendButton, dontSendButton},
      GroupLayout.VERTICAL
    );
*/

// FIXME: button width?
/*   
    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);
    panel.setBorder(BorderFactory.createEmptyBorder(17,12,12,12));

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(dontShowAgainCheckBox, c);

    c.gridx = 1;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(Box.createHorizontalGlue(), c);

    c.gridx = 2;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(dontSendButton, c);

    c.gridx = 3;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,5,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(sendButton, c);

    c.gridx = 4;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,5,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(cancelButton, c);

    c.gridx = 5;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,5,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(okButton, c);
*/
    return panel;     
  }

  private Component buildVersionCheckPanel() {
    final JXBusyLabel spinner = new JXBusyLabel(new Dimension(40,40));
    spinner.setBusy(true);

    final FlowLabel label =
      new FlowLabel(Resources.getString("BugDialog.collecting_details"));

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.BOTH;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(label, c);

    c.gridx = 0;
    c.gridy = 1;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 0.0;
    c.weighty = 1.0;
    panel.add(Box.createVerticalGlue(), c);

    c.gridx = 0;
    c.gridy = 2;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.CENTER;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(spinner, c);

    c.gridx = 0;
    c.gridy = 3;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 0.0;
    c.weighty = 1.0;
    panel.add(Box.createVerticalGlue(), c);

/*
    final JPanel panel = new JPanel();
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(label)
        .add(layout.createSequentialGroup()
          .add(0, 0, Integer.MAX_VALUE)
          .add(spinner)
          .add(0, 0, Integer.MAX_VALUE)));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .add(label)
        .add(0, 0, Integer.MAX_VALUE)
        .add(spinner)
        .add(0, 0, Integer.MAX_VALUE));
*/

    return panel;
  }

  private Component buildVersionCheckButtons() {
    final JButton cancelButton = new JButton(
      new AbstractAction(Resources.getString(Resources.CANCEL)) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      }
    );

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(Box.createHorizontalGlue(), c);

    c.gridx = 1;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(cancelButton, c);

    return panel;
  }

  private Component buildCurrentVersionPanel() {
    final FlowLabel label = new FlowLabel(
      Resources.getString("BugDialog.current_version_instructions"));
   
    descriptionArea = new JTextArea(10, 20);
    descriptionArea.setLineWrap(true);
    descriptionArea.setWrapStyleWord(true);

    final JScrollPane descriptionScroll = new JScrollPane(descriptionArea);

    final JLabel descriptionLabel =
      new JLabel(Resources.getString("BugDialog.bug_description"));
    descriptionLabel.setFont(
      descriptionLabel.getFont().deriveFont(Font.BOLD));
    descriptionLabel.setLabelFor(descriptionScroll);

    emailField = new JTextField(26);

    final JLabel emailLabel = new JLabel(
      Resources.getString("BugDialog.user_email_address"));
    emailLabel.setLabelFor(emailField);

    final JScrollPane detailsScroll = buildDetailsScroll();
    final JButton detailsButton = buildDetailsButton(detailsScroll);

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 2;
    c.gridheight = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(label, c);

    c.gridx = 0;
    c.gridy = 1;
    c.gridwidth = 2;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(12,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(descriptionLabel, c);

    c.gridx = 0;
    c.gridy = 2;
    c.gridwidth = 2;
    c.gridheight = 1;
    c.fill = GridBagConstraints.BOTH;
    c.insets = new Insets(6,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 1.0;
    panel.add(descriptionScroll, c);

    c.gridx = 0;
    c.gridy = 3;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(6,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(emailLabel, c);

    c.gridx = 1;
    c.gridy = 3;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(6,12,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(emailField, c);

    c.gridx = 0;
    c.gridy = 4;
    c.gridwidth = 2;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(12,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(detailsButton, c);

    c.gridx = 0;
    c.gridy = 5;
    c.gridwidth = 2;
    c.gridheight = 1;
    c.fill = GridBagConstraints.BOTH;
    c.insets = new Insets(6,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 5.0;
    panel.add(detailsScroll, c);

/*
    final JPanel detailsPanel = buildDetailsPanel();
 
    final JPanel panel = new JPanel();
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(label)
        .add(descriptionLabel)
        .add(descriptionScroll)
        .add(layout.createSequentialGroup()
          .add(emailLabel)
          .add(emailField))
        .add(detailsPanel));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .add(label)
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(descriptionLabel)
        .add(descriptionScroll)
        .add(layout.createParallelGroup(GroupLayout.BASELINE, true)
          .add(emailLabel)
          .add(emailField))
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(detailsPanel));
*/

    return panel;
  }

  private Component buildCurrentVersionButtons() {
    final JButton sendButton = new JButton(
      new AbstractAction(Resources.getString("BugDialog.send_button")) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          showSendingBugReportPanel();
        }
      }
    );

    final JButton dontSendButton = new JButton(
      new AbstractAction(Resources.getString("BugDialog.dont_send_button")) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          showEmergencySavePanel();
        }
      }
    );

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(dontSendButton, c);

    c.gridx = 1;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,5,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(sendButton, c);

    return panel;
  }

  private JButton buildDetailsButton(final JScrollPane detailsScroll) {
    final JButton detailsButton = new JButton();
    detailsButton.setBorderPainted(false);
    detailsButton.setContentAreaFilled(false);
    final Insets i = detailsButton.getInsets();
    detailsButton.setBorder(
      BorderFactory.createEmptyBorder(i.top, 0, i.bottom, i.right));
    detailsButton.setAction(
      new AbstractAction(Resources.getString("BugDialog.show_details"),
                         UIManager.getIcon("Tree.collapsedIcon")) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          final Dimension d = detailsScroll.getSize();

          final boolean visible = !detailsScroll.isVisible();
          detailsButton.setText(visible ?
            Resources.getString("BugDialog.hide_details") :
            Resources.getString("BugDialog.show_details"));
          detailsButton.setIcon(UIManager.getIcon(
            visible ? "Tree.expandedIcon" : "Tree.collapsedIcon"));
          detailsScroll.setVisible(visible);

          // ensure that the dialog width remains constant
          d.height = Integer.MAX_VALUE;
          detailsScroll.setMaximumSize(d);
          pack();
          detailsScroll.setMaximumSize(null);
        }
      }
    );

    return detailsButton;
  }

  private JScrollPane buildDetailsScroll() {
    final JTextArea detailsArea = new JTextArea(errorLog, 10, 20);
    detailsArea.setEditable(false);

    final JScrollPane detailsScroll = new JScrollPane(detailsArea);
    detailsScroll.setVisible(false);

    return detailsScroll;
  }

/*
  private Component buildDetailsPanel() {
    final JPanel panel = new JPanel();
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(false);

    final JTextArea logArea = new JTextArea(getErrorLog(), 10, 20);
    logArea.setEditable(false);

    final JScrollPane logScroll = new JScrollPane(logArea);
    logScroll.setVisible(false);

    final JButton detailsButton = new JButton();
    detailsButton.setBorderPainted(false);
    detailsButton.setContentAreaFilled(false);
    detailsButton.setBorder(BorderFactory.createEmptyBorder());
    detailsButton.setMargin(new Insets(0,0,0,0));
    detailsButton.setAction(
      new AbstractAction("Show details",
                         UIManager.getIcon("Tree.collapsedIcon")) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          final boolean visible = logScroll.isVisible();
          detailsButton.setText(visible ? "Show details" : "Hide details");
          detailsButton.setIcon(UIManager.getIcon(
            visible ? "Tree.collapsedIcon" : "Tree.expandedIcon"));
          logScroll.setVisible(!visible);
          pack();
        }
      }
    );

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(detailsButton)
        .add(logScroll));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .add(detailsButton)
        .add(0, 0, Integer.MAX_VALUE)
        .add(logScroll));

    return panel;
  }
*/

  private Component buildOldVersionPanel() {
/*
    final JPanel panel = new JPanel();
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);

    final Label label = new Label("VASSAL had an internal error. Because this version of VASSAL is no longer current, bug reporting is disabled. If you can reproduce this bug with a current verision of VASSAL, please do so and alert the VASSAL developers to the problem.");

    final JPanel detailsPanel = buildDetailsPanel();

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(label)
        .add(detailsPanel));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .add(label)
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(detailsPanel));
*/

    final FlowLabel label =
      new FlowLabel(Resources.getString("BugDialog.old_version_instructions"));
    label.addHyperlinkListener(BrowserSupport.getListener());

    final JScrollPane detailsScroll = buildDetailsScroll();
    final JButton detailsButton = buildDetailsButton(detailsScroll);

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(label, c);

    c.gridy = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(12,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(detailsButton, c);

    c.gridy = 2;
    c.fill = GridBagConstraints.BOTH;
    c.insets = new Insets(6,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 1.0;
    panel.add(detailsScroll, c);

    return panel;
  }

  private Component buildOldVersionButtons() {
    final JButton okButton = new JButton(
      new AbstractAction(Resources.getString(Resources.OK)) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          showEmergencySavePanel();
        }
      }
    );

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(Box.createHorizontalGlue(), c);

    c.gridx = 1;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(okButton, c);

    return panel;
  }

  private Component buildConnectionFailedPanel() {
    final String errorLogPath =
      new File(Info.getConfDir(), "errorLog").getAbsolutePath();
    final FlowLabel label = new FlowLabel(Resources.getString(
      "BugDialog.connection_failed_instructions", errorLogPath));
    label.addHyperlinkListener(BrowserSupport.getListener());

    final JScrollPane detailsScroll = buildDetailsScroll();
    final JButton detailsButton = buildDetailsButton(detailsScroll);
    
    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(label, c);

    c.gridy = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(12,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(detailsButton, c);

    c.gridy = 2;
    c.fill = GridBagConstraints.BOTH;
    c.insets = new Insets(6,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 1.0;
    panel.add(detailsScroll, c);

    c.gridy = 3;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 1.0;
    panel.add(Box.createVerticalGlue(), c);
    
    return panel;
  }

  private Component buildConnectionFailedButtons() {
    final JButton okButton = new JButton(
      new AbstractAction(Resources.getString(Resources.OK)) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          showEmergencySavePanel();
        }
      }
    );

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(Box.createHorizontalGlue(), c);

    c.gridx = 1;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(okButton, c);

    return panel;
  }

  private Component buildEmergencySavePanel() {
//      final FlowLabel label = new FlowLabel("Due to the error, VASSAL may be in an inconsistent state or may behave erratically. We recommend that you save copies of your open files using the \"Save\" button below, and restart VASSAL. Depending on what the error was, modules, saved games, and logs written after an error may be corrupt. Be sure to check any modules, saved games, or logs written after an error before continuing to use them.");
    final FlowLabel label =
      new FlowLabel(Resources.getString("BugDialog.how_to_proceed"));

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(label, c);

    c.gridy = 1;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 1.0;
    panel.add(Box.createVerticalGlue(), c);
    
    return panel;
  }

  private Component buildEmergencySaveButtons() {
/*
    final JButton saveButton = new JButton(
      new AbstractAction("Save") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          emergencySave();
          dispose();
        }
      }
    );

    final JButton dontSaveButton = new JButton(
      new AbstractAction("Don't Save") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      }
    );

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(dontSaveButton, c);

    c.gridx = 1;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,5,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(saveButton, c);
*/

    final JButton okButton = new JButton(
      new AbstractAction(Resources.getString(Resources.OK)) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      }
    );

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(Box.createHorizontalGlue(), c);

    c.gridx = 1;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(okButton, c);

    return panel;
  }

  private void showVersionCheckPanel() {
    deck.show(contents, "versionCheckPanel");
    button_deck.show(buttons, "versionCheckButtons");
/*
    sendButton.setVisible(false);
    dontSendButton.setVisible(false);
    okButton.setVisible(false);
    cancelButton.setVisible(true); 
*/
  }

  private void showCurrentVersionPanel() {
    deck.show(contents, "currentVersionPanel");
    button_deck.show(buttons, "currentVersionButtons");
/*
    sendButton.setVisible(true);
    dontSendButton.setVisible(true);
    okButton.setVisible(false);
    cancelButton.setVisible(false); 
*/
  }

  private void showSendingBugReportPanel() {
    deck.show(contents, "sendingBugReportPanel");
    button_deck.show(buttons, "sendingBugReportButtons");

    sendRequest = new SendRequest(); 
    sendRequest.execute();

/*
    sendButton.setVisible(true);
    dontSendButton.setVisible(true);
    okButton.setVisible(false);
    cancelButton.setVisible(false); 
*/
  }

  private void showOldVersionPanel() {
    deck.show(contents, "oldVersionPanel");
    button_deck.show(buttons, "oldVersionButtons");
/*
    sendButton.setVisible(false);
    dontSendButton.setVisible(false);
    okButton.setVisible(true);
    cancelButton.setVisible(false); 
*/
  }

  private void showConnectionFailedPanel() {
    deck.show(contents, "connectionFailedPanel");
    button_deck.show(buttons, "connectionFailedButtons");
/*
    wardsn.setVisible(false);
    dontSendButton.setVisible(false);
    okButton.setVisible(true);
    cancelButton.setVisible(false); 
*/
  }

  private void showEmergencySavePanel() {
    deck.show(contents, "emergencySavePanel");
    button_deck.show(buttons, "emergencySaveButtons");
/*
    sendButton.setVisible(false);
    dontSendButton.setVisible(false);
    okButton.setVisible(true);
    cancelButton.setVisible(false);
*/
  }

  private CheckRequest checkRequest = null;

  @Override
  public void setVisible(boolean visible) {
    if (visible && !isVisible()) {
      checkRequest = new CheckRequest();
      checkRequest.execute();
    }
    super.setVisible(visible);
  }

  private class CheckRequest extends SwingWorker<Boolean,Void> {
    private Timer timer = null;

    @Override
    protected Boolean doInBackground() throws Exception {
      final CountDownLatch latch = new CountDownLatch(1);

      // Wait 3 seconds before counting down the latch to ensure
      // that the user has sufficient time to read the message on
      // the first pane.
      timer = new Timer(2000, new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          latch.countDown();
        }
      });
      timer.start();

      // Make the request to the server and wait for the latch.
      final VassalVersion running = new VassalVersion(Info.getVersion());
      final Boolean cur = VersionUtils.isCurrent(running);
      latch.await();
      return cur;
    }

    @Override
    protected void done() {
      try {
        if (get(10, TimeUnit.SECONDS)) showCurrentVersionPanel();
//          else       showCurrentVersionPanel();
        else       showOldVersionPanel();
//          else       showConnectionFailedPanel();
      }
      catch (CancellationException e) {
        // cancelled by user, do nothing
        timer.stop();
      }
      catch (InterruptedException e) {
        timer.stop();
        e.printStackTrace();
        showConnectionFailedPanel();
      }
      catch (ExecutionException e) {
        timer.stop();
        e.printStackTrace();
        showConnectionFailedPanel();
      }
      catch (TimeoutException e) {
        timer.stop();
        e.printStackTrace();
        showConnectionFailedPanel();
      }
    }
  }

  private Component buildSendingBugReportPanel() {
    final JXBusyLabel spinner = new JXBusyLabel(new Dimension(40,40));
    spinner.setBusy(true);

    final FlowLabel label =
      new FlowLabel(Resources.getString("BugDialog.sending_bug_report"));

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.BOTH;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_START;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(label, c);

    c.gridx = 0;
    c.gridy = 1;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 0.0;
    c.weighty = 1.0;
    panel.add(Box.createVerticalGlue(), c);

    c.gridx = 0;
    c.gridy = 2;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.CENTER;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(spinner, c);

    c.gridx = 0;
    c.gridy = 3;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 0.0;
    c.weighty = 1.0;
    panel.add(Box.createVerticalGlue(), c);

/*
    final JPanel panel = new JPanel();
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(label)
        .add(layout.createSequentialGroup()
          .add(0, 0, Integer.MAX_VALUE)
          .add(spinner)
          .add(0, 0, Integer.MAX_VALUE)));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .add(label)
        .add(0, 0, Integer.MAX_VALUE)
        .add(spinner)
        .add(0, 0, Integer.MAX_VALUE));
*/

    return panel;
  }

  private Component buildSendingBugReportButtons() {
    final JButton cancelButton = new JButton(
      new AbstractAction(Resources.getString(Resources.CANCEL)) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      }
    );

    final GridBagConstraints c = new GridBagConstraints();
    final GridBagLayout layout = new GridBagLayout();
    final JPanel panel = new JPanel(layout);

    c.gridx = 0;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.NORTHWEST;
    c.weightx = 1.0;
    c.weighty = 0.0;
    panel.add(Box.createHorizontalGlue(), c);

    c.gridx = 1;
    c.gridy = 0;
    c.gridwidth = 1;
    c.gridheight = 1;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0,0,0,0);
    c.anchor = GridBagConstraints.LINE_END;
    c.weightx = 0.0;
    c.weighty = 0.0;
    panel.add(cancelButton, c);

    return panel;
  }

  private SendRequest sendRequest = null;

  private class SendRequest extends SwingWorker<Void,Void> {
    private Timer timer = null;

    @Override
    protected Void doInBackground() throws Exception {
      final CountDownLatch latch = new CountDownLatch(1);

      // Wait 3 seconds before counting down the latch to ensure
      // that the user has sufficient time to read the message on
      // the first pane.
      timer = new Timer(2000, new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          latch.countDown();
        }
      });
      timer.start();

      // Make the request to the server and wait for the latch.
      BugUtils.sendBugReport(
        emailField.getText(),
        descriptionArea.getText(),
        errorLog,
        thrown
      );

      latch.await();
      return null;
    }

    @Override
    protected void done() {
      try {
        get(10, TimeUnit.SECONDS);
        showEmergencySavePanel();
      }
      catch (CancellationException e) {
        // cancelled by user, do nothing
        timer.stop();
      }
      catch (InterruptedException e) {
        timer.stop();
        e.printStackTrace();
        showConnectionFailedPanel();
      }
      catch (ExecutionException e) {
        timer.stop();
        e.printStackTrace();
        showConnectionFailedPanel();
      }
      catch (TimeoutException e) {
        timer.stop();
        e.printStackTrace();
        showConnectionFailedPanel();
      }
    }
  }

// FIXME: add a page thanking the user for his bug report and providing
// a link to it at SF.

  private void emergencySave() {
// FIXME: GameModule and GameState need save methods which take a filename
/*      
    final GameModule mod = GameModule.getGameModule();
    if (mod != null) mod.save(false);

    final GameState state = mod.getGameState();
    if (state != null && state.isModified()) {
      state.saveGame();
    }
*/       
  }

  public static void main(String[] args) {
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        final BugDialog bd = new BugDialog(null, null);
        bd.setVisible(true);
      }
    });
  }
}
