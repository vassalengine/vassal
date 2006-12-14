package VASSAL.chat.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleStatus;

public class SimpleStatusControlsInitializer implements ChatControlsInitializer {
  private ChatServerConnection client;
  private JButton lookingBox;
  private JButton awayButton;
  
  public SimpleStatusControlsInitializer(ChatServerConnection client) {
    super();
    this.client = client;
  }

  public void initializeControls(final ChatServerControls controls) {
    lookingBox = new JButton("Looking for game");
    lookingBox.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        if (client != null) {
          Player p = client.getUserInfo();
          SimpleStatus s = (SimpleStatus) p.getStatus();
          s = new SimpleStatus(!s.isLooking(),s.isAway(),s.getProfile());
          client.setUserInfo(new SimplePlayer(p.getId(),p.getName(),s));
        }
      }
    });
    lookingBox.setSize(lookingBox.getMinimumSize());
    URL imageURL = getClass().getResource("/images/playerLooking.gif");
    if (imageURL != null) {
      lookingBox.setToolTipText(lookingBox.getText());
      lookingBox.setText("");
      lookingBox.setIcon(new ImageIcon(imageURL));
    }
    awayButton = new JButton("Away from keyboard");
    awayButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        if (client != null) {
          Player p = client.getUserInfo();
          SimpleStatus s = (SimpleStatus) p.getStatus();
          s = new SimpleStatus(s.isLooking(),true,s.getProfile());
          client.setUserInfo(new SimplePlayer(p.getId(),p.getName(),s));
          JOptionPane.showMessageDialog(controls.getRoomTree(), "I'm back", "Away from keyboard", JOptionPane.PLAIN_MESSAGE);
          s = (SimpleStatus) p.getStatus();
          s = new SimpleStatus(s.isLooking(),false ,s.getProfile());
          client.setUserInfo(new SimplePlayer(p.getId(),p.getName(),s));
        }
      }
    });
    imageURL = getClass().getResource("/images/playerAway.gif");
    if (imageURL != null) {
      awayButton.setToolTipText(awayButton.getText());
      awayButton.setText("");
      awayButton.setIcon(new ImageIcon(imageURL));
    }
    controls.getToolbar().add(lookingBox);
    controls.getToolbar().add(awayButton);
  }

  public void uninitializeControls(ChatServerControls controls) {
    controls.getToolbar().remove(lookingBox);
    controls.getToolbar().remove(awayButton);
  }
}
