/*
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.DetailsButton;
import VASSAL.tools.swing.FlowLabel;
import VASSAL.tools.version.VersionUtils;

import net.miginfocom.swing.MigLayout;

import org.jdesktop.swingx.JXBusyLabel;
import org.jdesktop.swingx.JXHeader;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.Timer;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class BugDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private static final Logger logger = LoggerFactory.getLogger(BugDialog.class);

  private final Throwable thrown;
  private final String errorLog;

  private JPanel contents;
  private CardLayout deck;

  private JPanel buttons;
  private CardLayout button_deck;

  private JTextArea descriptionArea;
  private JTextField emailField;

  private JScrollPane descriptionScroll;

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
      new ImageIcon(BugDialog.class.getResource("/icons/48x48/bug.png"))  //NON-NLS
    );

    //
    // dialog
    //
    setTitle(Resources.getString("BugDialog.title"));
    setLocationRelativeTo(owner);
    setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    setResizable(true);

    addWindowListener(new WindowAdapter() {
      @Override
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
    contents.setBorder(BorderFactory.createEmptyBorder(12, 12, 0, 12));

    contents.add(buildVersionCheckPanel(),     "versionCheckPanel");  //NON-NLS
    contents.add(buildCurrentVersionPanel(),   "currentVersionPanel"); //NON-NLS
    contents.add(buildSendingBugReportPanel(), "sendingBugReportPanel"); //NON-NLS
    contents.add(buildNonReportingVersionPanel("BugDialog.old_version_instructions"), "oldVersionPanel"); //NON-NLS
    contents.add(buildNonReportingVersionPanel("BugDialog.test_version_instructions"), "oldVersionPanel"); //NON-NLS
    contents.add(buildConnectionFailedPanel(), "connectionFailedPanel"); //NON-NLS
    contents.add(buildEmergencySavePanel(),    "emergencySavePanel"); //NON-NLS

    return contents;
  }

  private Component buildButtonPanel() {
    button_deck = new CardLayout();
    buttons = new JPanel(button_deck);

    buttons.add(buildVersionCheckButtons(),     "versionCheckButtons"); //NON-NLS
    buttons.add(buildCurrentVersionButtons(),   "currentVersionButtons"); //NON-NLS
    buttons.add(buildSendingBugReportButtons(), "sendingBugReportButtons"); //NON-NLS
    buttons.add(buildNonReportingVersionButtons(), "oldVersionButtons"); //NON-NLS
    buttons.add(buildNonReportingVersionButtons(), "testVersionButtons"); //NON-NLS
    buttons.add(buildConnectionFailedButtons(), "connectionFailedButtons"); //NON-NLS
    buttons.add(buildEmergencySaveButtons(),    "emergencySaveButtons"); //NON-NLS

    return buttons;
  }

  private Component buildVersionCheckPanel() {
    final JXBusyLabel spinner = new JXBusyLabel(new Dimension(40, 40));
    spinner.setBusy(true);

    final FlowLabel label =
      new FlowLabel(Resources.getString("BugDialog.collecting_details"));

    final JPanel panel = new JPanel(
      new MigLayout("", "", "[]push[]push"));  //NON-NLS
    panel.add(label, "cell 0 0, growx, pushx"); //NON-NLS
    panel.add(spinner, "cell 0 1, align center"); //NON-NLS

    return panel;
  }

  private Component buildVersionCheckButtons() {
    final JButton cancelButton = new JButton(
      new AbstractAction(Resources.getString(Resources.CANCEL)) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      }
    );

// FIXME: tags don't push buttons to ends?
    final JPanel panel = new JPanel(new MigLayout("align right")); //NON-NLS
    panel.add(cancelButton, "tag cancel"); //NON-NLS

    return panel;
  }

  private Component buildCurrentVersionPanel() {
    final FlowLabel label = new FlowLabel(
      Resources.getString("BugDialog.current_version_instructions"));

    descriptionArea = new JTextArea(10, 1);
    descriptionArea.setLineWrap(true);
    descriptionArea.setWrapStyleWord(true);

    descriptionScroll = new JScrollPane(descriptionArea);

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

    final DetailsButton detailsButton = new DetailsButton(
      Resources.getString("Dialogs.show_details"),
      Resources.getString("Dialogs.hide_details"),
      detailsScroll
    );
    detailsButton.setBuddy(label);

    final JPanel panel = new JPanel(new MigLayout(
      "hidemode 3", "", "[]unrel[]rel[]unrel[]unrel[]rel[]")); //NON-NLS
    panel.add(label, "cell 0 0, growx, pushx"); //NON-NLS
    panel.add(descriptionLabel, "cell 0 1"); //NON-NLS
    panel.add(descriptionScroll, "cell 0 2, grow, push"); //NON-NLS
    panel.add(emailLabel, "cell 0 3"); //NON-NLS
    panel.add(emailField, "cell 0 3, growx, pushx"); //NON-NLS
    panel.add(detailsButton, "cell 0 4"); //NON-NLS
    panel.add(detailsScroll, "cell 0 5, grow, push"); //NON-NLS

    return panel;
  }

  private Component buildCurrentVersionButtons() {
    final JButton sendButton = new JButton(
      new AbstractAction(Resources.getString("BugDialog.send_button")) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          showSendingBugReportPanel();
        }
      }
    );

    final JButton dontSendButton = new JButton(
      new AbstractAction(Resources.getString("BugDialog.dont_send_button")) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          showEmergencySavePanel();
        }
      }
    );

    final JPanel panel = new JPanel(new MigLayout("align right")); //NON-NLS
    panel.add(sendButton, "tag ok"); //NON-NLS
    panel.add(dontSendButton, "tag cancel"); //NON-NLS

    return panel;
  }

  private JScrollPane buildDetailsScroll() {
    final JTextArea detailsArea = new JTextArea(errorLog, 10, 20);
    detailsArea.setEditable(false);
    detailsArea.setTabSize(2);
    return new JScrollPane(detailsArea);
  }

  private Component buildNonReportingVersionPanel(String key) {
    final FlowLabel label = new FlowLabel(Resources.getString(key));
    label.addHyperlinkListener(BrowserSupport.getListener());

    final JScrollPane detailsScroll = buildDetailsScroll();

    final DetailsButton detailsButton = new DetailsButton(
      Resources.getString("Dialogs.show_details"),
      Resources.getString("Dialogs.hide_details"),
      detailsScroll
    );
    detailsButton.setBuddy(label);

    final JPanel panel = new JPanel(new MigLayout(
      "hidemode 3", "", "[]unrel[]rel[]")); //NON-NLS
    panel.add(label, "cell 0 0, growx, pushx"); //NON-NLS
    panel.add(detailsButton, "cell 0 1"); //NON-NLS
    panel.add(detailsScroll, "cell 0 2, grow, push"); //NON-NLS

    return panel;
  }

  private Component buildNonReportingVersionButtons() {
    final JButton okButton = new JButton(
      new AbstractAction(Resources.getString(Resources.OK)) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          showEmergencySavePanel();
        }
      }
    );

    final JPanel panel = new JPanel(new MigLayout("align right")); //NON-NLS
    panel.add(okButton, "tag ok"); //NON-NLS

    return panel;
  }

  private Component buildConnectionFailedPanel() {
    final String errorLogPath = Info.getErrorLogPath().getAbsolutePath();

    final FlowLabel label = new FlowLabel(Resources.getString(
      "BugDialog.connection_failed_instructions", errorLogPath));
    label.addHyperlinkListener(BrowserSupport.getListener());

    final JScrollPane detailsScroll = buildDetailsScroll();

    final DetailsButton detailsButton = new DetailsButton(
      Resources.getString("Dialogs.show_details"),
      Resources.getString("Dialogs.hide_details"),
      detailsScroll
    );
    detailsButton.setBuddy(label);

    final JPanel panel = new JPanel(new MigLayout(
      "hidemode 3", "", "[]unrel[]rel[]")); //NON-NLS
    panel.add(label, "cell 0 0, growx, pushx"); //NON-NLS
    panel.add(detailsButton, "cell 0 1"); //NON-NLS
    panel.add(detailsScroll, "cell 0 2, grow, push");  //NON-NLS

    return panel;
  }

  private Component buildConnectionFailedButtons() {
    final JButton okButton = new JButton(
      new AbstractAction(Resources.getString(Resources.OK)) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          showEmergencySavePanel();
        }
      }
    );

    final JPanel panel = new JPanel(new MigLayout("align right")); //NON-NLS
    panel.add(okButton, "tag ok"); //NON-NLS

    return panel;
  }

  private Component buildEmergencySavePanel() {
    final FlowLabel label =
      new FlowLabel(Resources.getString("BugDialog.how_to_proceed"));

    final JPanel panel = new JPanel(new MigLayout("", "", "[]push")); //NON-NLS
    panel.add(label, "cell 0 0, growx, pushx"); //NON-NLS

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
*/

    final JButton okButton = new JButton(
      new AbstractAction(Resources.getString(Resources.OK)) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      }
    );

    final JPanel panel = new JPanel(new MigLayout("align right")); //NON-NLS
    panel.add(okButton, "tag ok"); //NON-NLS

    return panel;
  }

  private void showVersionCheckPanel() {
    deck.show(contents, "versionCheckPanel");
    button_deck.show(buttons, "versionCheckButtons");
  }

  private void showCurrentVersionPanel() {
    deck.show(contents, "currentVersionPanel");
    button_deck.show(buttons, "currentVersionButtons");
  }

  private void showSendingBugReportPanel() {
    deck.show(contents, "sendingBugReportPanel");
    button_deck.show(buttons, "sendingBugReportButtons");

    sendRequest = new SendRequest();
    sendRequest.execute();
  }

  private void showOldVersionPanel() {
    deck.show(contents, "oldVersionPanel");
    button_deck.show(buttons, "oldVersionButtons");
  }

  private void showTestVersionPanel() {
    deck.show(contents, "testVersionPanel");
    button_deck.show(buttons, "testVersionButtons");
  }

  private void showConnectionFailedPanel() {
    deck.show(contents, "connectionFailedPanel");
    button_deck.show(buttons, "connectionFailedButtons");
  }

  private void showEmergencySavePanel() {
    deck.show(contents, "emergencySavePanel");
    button_deck.show(buttons, "emergencySaveButtons");
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

  private class CheckRequest extends SwingWorker<Integer, Void> {
    private Timer timer = null;

    @Override
    protected Integer doInBackground() throws Exception {
      final CountDownLatch latch = new CountDownLatch(1);

      // Wait 3 seconds before counting down the latch to ensure
      // that the user has sufficient time to read the message on
      // the first pane.
      timer = new Timer(2000, e -> latch.countDown());
      timer.start();

      // Make the request to the server and wait for the latch.
      final int cur = VersionUtils.compareReportable(Info.getVersion());
      latch.await();
      return cur;
    }

    @Override
    protected void done() {
      try {
        final Integer v = get(10, TimeUnit.SECONDS);

        if (v == 0) {
          showCurrentVersionPanel();
        }
        else if (v < 0) {
          showOldVersionPanel();
        }
        else {
          showTestVersionPanel();
        }
      }
      catch (CancellationException e) {
        // cancelled by user, do nothing
        timer.stop();
      }
      catch (InterruptedException | TimeoutException | ExecutionException e) {
        timer.stop();
        logger.error("", e);
        showConnectionFailedPanel();
      }
    }
  }

  private Component buildSendingBugReportPanel() {
    final JXBusyLabel spinner = new JXBusyLabel(new Dimension(40, 40));
    spinner.setBusy(true);

    final FlowLabel label =
      new FlowLabel(Resources.getString("BugDialog.sending_bug_report"));

    final JPanel panel = new JPanel(
      new MigLayout("", "", "[]push[]push")); //NON-NLS
    panel.add(label, "cell 0 0, growx, pushx"); //NON-NLS
    panel.add(spinner, "cell 0 1, align center"); //NON-NLS

    return panel;
  }

  private Component buildSendingBugReportButtons() {
    final JButton cancelButton = new JButton(
      new AbstractAction(Resources.getString(Resources.CANCEL)) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      }
    );

    final JPanel panel = new JPanel(new MigLayout("align right")); //NON-NLS
    panel.add(cancelButton, "tag cancel"); //NON-NLS

    return panel;
  }

  private SendRequest sendRequest = null;

  private class SendRequest extends SwingWorker<Void, Void> {
    private Timer timer = null;

    @Override
    protected Void doInBackground() throws Exception {
      final CountDownLatch latch = new CountDownLatch(1);

      // Wait 3 seconds before counting down the latch to ensure
      // that the user has sufficient time to read the message on
      // the first pane.
      timer = new Timer(2000, e -> latch.countDown());
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
      catch (InterruptedException | TimeoutException | ExecutionException e) {
        timer.stop();
        logger.error("", e);
        showConnectionFailedPanel();
      }
    }
  }

// FIXME: add a page thanking the user for his bug report and providing
// a link to it at SF.

/*
  private void emergencySave() {
// FIXME: GameModule and GameState need save methods which take a filename
    final GameModule mod = GameModule.getGameModule();
    if (mod != null) mod.save(false);

    final GameState state = mod.getGameState();
    if (state != null && state.isModified()) {
      state.saveGame();
    }
  }
*/

  public static void main(String[] args) {
    SwingUtilities.invokeLater(() -> {
      final BugDialog bd = new BugDialog(null, null);
      bd.setVisible(true);
    });
  }
}
