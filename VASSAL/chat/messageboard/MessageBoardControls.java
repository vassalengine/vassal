package VASSAL.chat.messageboard;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;
import java.util.Vector;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

/**
 * UI controls for posting and retrieving messages from a
 * message board.
 */
public class MessageBoardControls {
  private MessageBoard server;
  private String serverName;
  private Action checkMessagesAction;
  private Action postMessageAction;
  private MessageViewer viewer;
  private JFrame msgFrame;
  private JFrame msgComposer;

  public MessageBoardControls() {
    initComponents();
    setServer(null, null);
  }

  public void setServer(MessageBoard msgSvr, String name) {
    server = msgSvr;
    serverName = name;
    checkMessagesAction.setEnabled(server != null);
    postMessageAction.setEnabled(server != null);
  }

  public void showMessages(Message[] m) {
    Vector v = new Vector();
    for (int i = 0; i < m.length; ++i) {
      v.addElement(m[i]);
    }
    viewer.setMessages(v.elements());
  }

  private void initComponents() {
    viewer = new MessageViewer();

    msgFrame = new JFrame("Message Board");
    msgFrame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
    msgFrame.getContentPane().setLayout(new BoxLayout(msgFrame.getContentPane(), BoxLayout.Y_AXIS));
    msgFrame.getContentPane().add(viewer);

    Box box = Box.createHorizontalBox();
    JButton b = new JButton("Refresh");
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        msgFrame.setTitle(serverName != null ? serverName+" Message Board" : "Message Board");
        showMessages(server.getMessages());
      }
    });
    box.add(b);
    b = new JButton("Close");
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        msgFrame.setVisible(false);
      }
    });
    box.add(b);
    msgFrame.getContentPane().add(box);

    msgComposer = new Comp();

    checkMessagesAction = new AbstractAction("Check Messages") {
      public void actionPerformed(ActionEvent evt) {
        msgFrame.setTitle(serverName != null ? serverName+" Message Board" : "Message Board");
        showMessages(server.getMessages());
        msgFrame.setVisible(true);
      }
    };

    URL imageURL = getClass().getResource("/images/getMessages.gif");
    if (imageURL != null) {
      checkMessagesAction.putValue(Action.SHORT_DESCRIPTION, checkMessagesAction.getValue(Action.NAME));
      checkMessagesAction.putValue(Action.NAME, "");
      checkMessagesAction.putValue(Action.SMALL_ICON, new ImageIcon(imageURL));
    }


    postMessageAction = new AbstractAction("Post Message") {
      public void actionPerformed(ActionEvent evt) {
        msgComposer.setVisible(true);
      }
    };
    imageURL = getClass().getResource("/images/postMessage.gif");
    if (imageURL != null) {
      postMessageAction.putValue(Action.SHORT_DESCRIPTION, postMessageAction.getValue(Action.NAME));
      postMessageAction.putValue(Action.NAME, "");
      postMessageAction.putValue(Action.SMALL_ICON, new ImageIcon(imageURL));
    }

    msgFrame.pack();
    msgFrame.setLocation(java.awt.Toolkit.getDefaultToolkit().getScreenSize().width / 2 - msgFrame.getSize().width / 2, 0);
  }

  public Action getCheckMessagesAction() {
    return checkMessagesAction;
  }

  public Action getPostMessageAction() {
    return postMessageAction;
  }

  private class Comp extends JFrame {
    private Comp() {
      super("Message Composer");
      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
      final JTextArea msgArea = new JTextArea(15, 60);
      Box b = Box.createHorizontalBox();

      JButton okButton = new JButton("Send");
      okButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          server.postMessage(msgArea.getText());
          setVisible(false);
          msgArea.setText("");
        }
      });
      JButton cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          setVisible(false);
          msgArea.setText("");
        }
      });

      b.add(okButton);
      b.add(cancelButton);
      getContentPane().add(new JScrollPane(msgArea));
      getContentPane().add(b);

      pack();
      setLocation(java.awt.Toolkit.getDefaultToolkit().getScreenSize().width / 2 - getSize().width / 2, 0);
    }
  }
}
