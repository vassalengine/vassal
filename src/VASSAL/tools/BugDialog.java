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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.GregorianCalendar;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import org.jdesktop.layout.GroupLayout;
import org.jdesktop.layout.LayoutStyle;

import VASSAL.Info;
import VASSAL.tools.swing.FlowLabel;

/**
 * 
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class BugDialog {
  private static final long serialVersionUID = 1L;
  private static boolean bugReportingDisabled;

  public static void reportABug() {
/*
    reportABug(GameModule.getGameModule() == null
      ? null : GameModule.getGameModule().getFrame());
*/
// FIXME: do something to find our top-level frame

    final GregorianCalendar expiry = new GregorianCalendar(2008, 8, 5);
    final GregorianCalendar now = new GregorianCalendar();

    if (now.after(expiry)) oldReportABug(null);
    else reportABug(null);
  }

  public static void oldReportABug(Frame parent) {
    final JDialog dialog;

    final JLabel header = new JLabel("Congratulations! You've found a bug.");
    final Font f = header.getFont();
    header.setFont(f.deriveFont(Font.BOLD, f.getSize()*1.2f));

    final FlowLabel notice = new FlowLabel("VASSAL had an internal error. Because this beta version of VASSAL is more than 30 days old, bug reporting is disabled. If you can reproduce this bug with a current verision of VASSAL, please do so and alert the VASSAL developers to the problem.");

    // prevents FlowLabel from being a single line
    // FIXME: why is this necessary?
    final Dimension d = notice.getPreferredSize();
    d.width = 0;
    notice.setPreferredSize(d);

    String tmp = null;
    FileReader r = null;
    try {
      r = new FileReader(new File(Info.getConfDir(), "errorLog"));
      tmp = IOUtils.toString(r);
    }
    catch (IOException e) {
      // Don't bother logging this---if we can't read the errorLog,
      // then we probably can't write to it either.
      IOUtils.closeQuietly(r);
    }

    final String errorLog = tmp;
  
    final JTextArea log = new JTextArea(errorLog, 10, 26);
    log.setEditable(false);

    final JScrollPane logScroll = new JScrollPane(log);
    logScroll.setVisible(false);

    final JLabel logLabel = new JLabel("Error Log:");
    logLabel.setLabelFor(log);

    final JButton detailsButton = new JButton();

    final JLabel detailsLabel = new JLabel("Error Log:");
    detailsLabel.setLabelFor(detailsButton); 

    final JPanel panel = new JPanel();

    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);
    
    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(header)
        .add(notice)
        .add(layout.createSequentialGroup()
          .add(layout.createParallelGroup(GroupLayout.LEADING, true)
            .add(detailsLabel))
          .add(layout.createParallelGroup(GroupLayout.LEADING, true)
            .add(detailsButton)
            .add(logScroll))));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .add(header)
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(layout.createParallelGroup(GroupLayout.BASELINE, false)
          .add(notice))
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(layout.createParallelGroup(GroupLayout.BASELINE, true)
          .add(detailsLabel)
          .add(detailsButton))
        .add(logScroll));

    final String[] buttons = { "Ok" };
    
    final JOptionPane opt = new JOptionPane(
      panel,
      JOptionPane.ERROR_MESSAGE,
      JOptionPane.DEFAULT_OPTION,
      new ImageIcon(BugDialog.class.getResource("/images/bug.png")),
      buttons
    );

    dialog = opt.createDialog(parent, "Uncaught Exception");

    detailsButton.setAction(new AbstractAction("<html>Details &raquo;</html>") {
      private static final long serialVersionUID = 1L;
    
      public void actionPerformed(ActionEvent e) {
        logScroll.setVisible(!logScroll.isVisible());
        putValue(NAME, logScroll.isVisible() ?
          "<html>Details &laquo;</html>" : "<html>Details &raquo;</html>");
        dialog.pack(); 
      }
    });

    dialog.setModal(true);
    dialog.setLocationRelativeTo(parent);
    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.setResizable(true);
    dialog.pack();
    dialog.setVisible(true);
  }

  public static void reportABug(Frame parent) {
    if (bugReportingDisabled) {
      return;
    }
    final JDialog dialog;

    final JLabel header = new JLabel("Congratulations! You've found a bug.");
    final Font f = header.getFont();
    header.setFont(f.deriveFont(Font.BOLD, f.getSize()*1.2f));

    final FlowLabel notice = new FlowLabel("VASSAL had an internal error. You can help the VASSAL developers fix it by sending them this error report with a description of what you were doing when the error happened.");
    notice.setEditable(false);

    // prevents FlowLabel from being a single line
    // FIXME: why is this necessary?
    final Dimension d = notice.getPreferredSize();
    d.width = 0;
    notice.setPreferredSize(d);

    final JTextArea description = new JTextArea(10, 20);
    description.setLineWrap(true);
    description.setWrapStyleWord(true);

    final JScrollPane descriptionScroll = new JScrollPane(description);
    descriptionScroll.setPreferredSize(description.getPreferredSize());

    final JLabel descriptionLabel = new JLabel("Description:");
    descriptionLabel.setLabelFor(description);

    final JTextField email = new JTextField(26);

//    final JLabel emailLabel = new JLabel("Email (Optional):"); 
    final JLabel emailLabel = new JLabel("Your Email:"); 
    emailLabel.setLabelFor(email);

    String tmp = null;
    FileReader r = null;
    try {
      r = new FileReader(new File(Info.getConfDir(), "errorLog"));
      tmp = IOUtils.toString(r);
    }
    catch (IOException e) {
      // Don't bother logging this---if we can't read the errorLog,
      // then we probably can't write to it either.
      IOUtils.closeQuietly(r);
    }

    final String errorLog = tmp;
  
    final JTextArea log = new JTextArea(errorLog, 10, 20);
    log.setEditable(false);

    final JScrollPane logScroll = new JScrollPane(log);
    logScroll.setVisible(false);

    final JLabel logLabel = new JLabel("Error Log:");
    logLabel.setLabelFor(log);

    final JButton detailsButton = new JButton();

    final JLabel detailsLabel = new JLabel("Error Log:");
    detailsLabel.setLabelFor(detailsButton); 

    final JPanel panel = new JPanel();

    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);
    
    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(header)
        .add(notice)
        .add(layout.createSequentialGroup()
          .add(layout.createParallelGroup(GroupLayout.LEADING, true)
            .add(descriptionLabel)
            .add(emailLabel)
            .add(detailsLabel))
          .add(layout.createParallelGroup(GroupLayout.LEADING, true)
            .add(descriptionScroll)
            .add(email)
            .add(detailsButton)
            .add(logScroll))));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .add(header)
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(layout.createParallelGroup(GroupLayout.BASELINE, false)
          .add(notice))
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(layout.createParallelGroup(GroupLayout.BASELINE, true)
          .add(descriptionLabel)
          .add(descriptionScroll))
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(layout.createParallelGroup(GroupLayout.BASELINE, true)
          .add(emailLabel)
          .add(email))
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(layout.createParallelGroup(GroupLayout.BASELINE, true)
          .add(detailsLabel)
          .add(detailsButton))
        .add(logScroll));

    layout.linkSize(new Component[]{emailLabel, email}, GroupLayout.VERTICAL);

    final String[] buttons = { "Submit Report", "Cancel", "Don't show this dialog again"};
    
    final JOptionPane opt = new JOptionPane(
      panel,
      JOptionPane.ERROR_MESSAGE,
      JOptionPane.DEFAULT_OPTION,
      new ImageIcon(BugDialog.class.getResource("/images/bug.png")),
      buttons
    );

    dialog = opt.createDialog(parent, "Uncaught Exception");

    detailsButton.setAction(new AbstractAction("<html>Details &raquo;</html>") {
      private static final long serialVersionUID = 1L;
    
      public void actionPerformed(ActionEvent e) {
        logScroll.setVisible(!logScroll.isVisible());
        putValue(NAME, logScroll.isVisible() ?
          "<html>Details &laquo;</html>" : "<html>Details &raquo;</html>");
        dialog.pack(); 
      }
    });

    dialog.setModal(true);
    dialog.setLocationRelativeTo(parent);
    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.setResizable(true);
    dialog.pack();
    dialog.setVisible(true);

    final Object selected = opt.getValue();
    if (buttons[0].equals(selected)) {
      sendBugReport(email.getText(), description.getText(), errorLog);
    }
    else if (buttons[2].equals(selected)) {
      bugReportingDisabled=true;
    }
  }

  private static void sendBugReport(String email,
                                    String description,
                                    String errorLog) {
    final long time = System.currentTimeMillis();
    final String summary = "Automated bug report #" + time + "\n";

    final String boundary = "---------------------------" +
                            Long.toString(time, 16);

    StringBuilder sb = new StringBuilder("Submitted by ");
    sb.append(email).append("\n\n").append(description);
    description = sb.toString();

    sb = new StringBuilder();
    writeParam("group_id", "90612", sb, boundary);
    writeParam("atid", "594231", sb, boundary);
    writeParam("func", "postadd", sb, boundary);
    writeParam("category_id", "100", sb, boundary);
    writeParam("artifact_group_id", "100", sb, boundary);
    writeParam("summary", summary, sb, boundary);
    writeParam("details", description, sb, boundary);
    writeFile("input_file", "errorLog", "application/octet-stream", errorLog, sb, boundary);
    writeParam("file_description", "the errorLog", sb, boundary);
    writeParam("submit", "SUBMIT", sb, boundary);
    writeEnd(sb, boundary);

    final String content = sb.toString();

    DataOutputStream out = null;
    DataInputStream in = null;
    try {
      final URL url = new URL("http://sourceforge.net/tracker/index.php");    
      final HttpURLConnection http = (HttpURLConnection) url.openConnection();
      http.setDoInput(true);
      http.setDoOutput(true);
      http.setUseCaches(false);
      http.setAllowUserInteraction(false);

      http.setRequestMethod("POST");

      http.setRequestProperty(
        "Content-Type", "multipart/form-data; boundary=" + boundary);
      http.setRequestProperty(
        "Content-Length", String.valueOf(content.length()));

      out = new DataOutputStream(http.getOutputStream()); 
      out.writeBytes(content);
      out.flush();
      out.close(); 
      
      in = new DataInputStream(http.getInputStream());
      IOUtils.copy(in, System.err);
      in.close();
    }
    // FIXME: review error message
    catch (IOException e) {
      // If we get here, then there is no path from the user to SourceForge.
      e.printStackTrace();
      System.err.println("\n");
      System.err.println(content);      

      final JLabel msg = new JLabel("<html>VASSAL was unable to submit your bug report. Please post the file \"" + /* Info.getHomeDir() */ "/home/uckelman/VASSAL/bugReport" + "\" in the Technical Support &amp; Bugs section of the <a href=\"http://forums.vassalengine.org/viewforum.php?f=3\">VASSAL Forum</a>.</html>");

      final JDialog d = new JOptionPane(msg, JOptionPane.ERROR_MESSAGE,
                           JOptionPane.DEFAULT_OPTION)
      .createDialog((Component) null, "Error");
  
      msg.setPreferredSize(new Dimension(400, 100));
      d.pack();
      d.setVisible(true);
    }
    finally {
      IOUtils.closeQuietly(out);
      IOUtils.closeQuietly(in);
    }
  }

  public static void main(String[] args) {
/*
    final JPanel panel = new JPanel();
    
    final JLabel msg1 = new JLabel("<html>VASSAL was unable to submit your bug report. Please post the file");

    final JXHyperlink fileLink = new JXHyperlink(
      new LinkAction("/home/uckelman/VASSAL/bugReport") {
        public void actionPerformed(ActionEvent e) {
          setVisited(true);
        }
      }
    );

    final JLabel msg2 = new JLabel("in the Technical Support & Bugs section of the ");
    
    final JXHyperlink forumLink = new JXHyperlink(
      new LinkAction("VASSAL Forum") {
        public void actionPerformed(ActionEvent e) {
          setVisited(true);
//<a href=\"http://forums.vassalengine.org/viewforum.php?f=3\">VASSAL Forum</a>.</html>");
        }
      }
    );

    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);
    
    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(msg1)
        .add(layout.createSequentialGroup()
          .add(0, 0, Integer.MAX_VALUE)
          .add(fileLink)
          .add(0, 0, Integer.MAX_VALUE))
        .add(layout.createSequentialGroup()
          .add(msg2)
          .add(0)
          .add(forumLink)));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .add(msg1)
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(fileLink)
        .addPreferredGap(LayoutStyle.UNRELATED)
        .add(layout.createParallelGroup(GroupLayout.BASELINE, true)
          .add(msg2)
          .add(forumLink)));

      final JDialog d = new JOptionPane(panel, JOptionPane.ERROR_MESSAGE,
                           JOptionPane.DEFAULT_OPTION).createDialog((Component) null, "Error");
    d.setResizable(true);
    d.pack();
    d.setVisible(true);
*/
    oldReportABug(null);
    reportABug();
    System.exit(0);
  }

  private static void writeParam(String name, String value, StringBuilder sb, String boundary) {
    sb.append("--").append(boundary).append("\r\n");
    sb.append("Content-Disposition: form-data; name=\"").append(name).append("\"\r\n\r\n");
    sb.append(value);
    sb.append("\r\n");
  }

  private static void writeFile(String name, String filename, String type, String content, StringBuilder sb, String boundary) {
    sb.append("--").append(boundary).append("\r\n");
    sb.append("Content-Disposition: form-data; name=\"").append(name).append("\"; filename=\"").append(filename).append("\"\r\n");
    sb.append("Content-Type: ").append(type).append("\r\n\r\n");
    sb.append(content);
    sb.append("\r\n");
  }

  private static void writeEnd(StringBuilder sb, String boundary) {
    sb.append("--").append(boundary).append("--").append("\r\n");
  }
}
