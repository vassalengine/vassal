/*
 * $Id$
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

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

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
import javax.swing.Timer;

import net.miginfocom.swing.MigLayout;

import org.jdesktop.swingworker.SwingWorker;
import org.jdesktop.swingx.JXBusyLabel;
import org.jdesktop.swingx.JXHeader;

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.DetailsButton;
import VASSAL.tools.swing.FlowLabel;
import VASSAL.tools.version.VassalVersion;
import VASSAL.tools.version.VersionUtils;

/**
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class BugDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private Throwable thrown;
  private String errorLog;

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
      new ImageIcon(BugDialog.class.getResource("/icons/48x48/bug.png"))
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

    return buttons;
  }

  private Component buildVersionCheckPanel() {
    final JXBusyLabel spinner = new JXBusyLabel(new Dimension(40,40));
    spinner.setBusy(true);

    final FlowLabel label =
      new FlowLabel(Resources.getString("BugDialog.collecting_details"));

    final JPanel panel = new JPanel(
      new MigLayout("", "", "[]push[]push"));
    panel.add(label, "cell 0 0, growx, pushx");
    panel.add(spinner, "cell 0 1, align center");

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

// FIXME: tags don't push buttons to ends?
    final JPanel panel = new JPanel(new MigLayout("align right"));
    panel.add(cancelButton, "tag cancel");

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
      "hidemode 3", "", "[]unrel[]rel[]unrel[]unrel[]rel[]"));
    panel.add(label, "cell 0 0, growx, pushx");
    panel.add(descriptionLabel, "cell 0 1");
    panel.add(descriptionScroll, "cell 0 2, grow, push");
    panel.add(emailLabel, "cell 0 3");
    panel.add(emailField, "cell 0 3, growx, pushx");
    panel.add(detailsButton, "cell 0 4");
    panel.add(detailsScroll, "cell 0 5, grow, push");

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

    final JPanel panel = new JPanel(new MigLayout("align right"));
    panel.add(sendButton, "tag ok");
    panel.add(dontSendButton, "tag cancel");

    return panel;
  }

  private JScrollPane buildDetailsScroll() {
    final JTextArea detailsArea = new JTextArea(errorLog, 10, 20);
    detailsArea.setEditable(false);
    detailsArea.setTabSize(2);
    return new JScrollPane(detailsArea);
  }

  private Component buildOldVersionPanel() {
    final FlowLabel label = new FlowLabel(
      Resources.getString("BugDialog.old_version_instructions"));
    label.addHyperlinkListener(BrowserSupport.getListener());

    final JScrollPane detailsScroll = buildDetailsScroll();

    final DetailsButton detailsButton = new DetailsButton(
      Resources.getString("Dialogs.show_details"),
      Resources.getString("Dialogs.hide_details"),
      detailsScroll
    );
    detailsButton.setBuddy(label);

    final JPanel panel = new JPanel(new MigLayout(
      "hidemode 3", "", "[]unrel[]rel[]"));
    panel.add(label, "cell 0 0, growx, pushx");
    panel.add(detailsButton, "cell 0 1");
    panel.add(detailsScroll, "cell 0 2, grow, push");

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

    final JPanel panel = new JPanel(new MigLayout("align right"));
    panel.add(okButton, "tag ok");

    return panel;
  }

  private Component buildConnectionFailedPanel() {
    final String errorLogPath =
      new File(Info.getConfDir(), "errorLog").getAbsolutePath();

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
      "hidemode 3", "", "[]unrel[]rel[]"));
    panel.add(label, "cell 0 0, growx, pushx");
    panel.add(detailsButton, "cell 0 1");
    panel.add(detailsScroll, "cell 0 2, grow, push");

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

    final JPanel panel = new JPanel(new MigLayout("align right"));
    panel.add(okButton, "tag ok");

    return panel;
  }

  private Component buildEmergencySavePanel() {
    final FlowLabel label =
      new FlowLabel(Resources.getString("BugDialog.how_to_proceed"));

    final JPanel panel = new JPanel(new MigLayout("", "", "[]push"));
    panel.add(label, "cell 0 0, growx, pushx");

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

        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      }
    );

    final JPanel panel = new JPanel(new MigLayout("align right"));
    panel.add(okButton, "tag ok");

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

    final JPanel panel = new JPanel(
      new MigLayout("", "", "[]push[]push"));
    panel.add(label, "cell 0 0, growx, pushx");
    panel.add(spinner, "cell 0 1, align center");

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

    final JPanel panel = new JPanel(new MigLayout("align right"));
    panel.add(cancelButton, "tag cancel");

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
