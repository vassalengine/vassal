package VASSAL.chat.peer2peer;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.Enumeration;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.WindowConstants;
import org.litesoft.p2pchat.PeerInfo;
import org.litesoft.p2pchat.PendingPeerManager;
import VASSAL.build.GameModule;
import VASSAL.chat.HttpRequestWrapper;

/**
 * Date: Mar 12, 2003
 */
public class DirectPeerPool implements PeerPool {
  private AcceptPeerThread acceptThread;
  private JDialog frame;
  private String myIp;

  public DirectPeerPool() {
  }

  public void initialize(P2PPlayer myInfo, PendingPeerManager ppm) throws IOException {
    myIp = myInfo.getInfo().getAddresses();
    try {
      myIp = discoverMyIpAddressFromRemote();
    }
    catch (IOException e) {
    }
    acceptThread = new AcceptPeerThread(myInfo.getInfo().getPort(), ppm);
    myInfo.getInfo().setPort(acceptThread.getPort());
    acceptThread.start();
    if (frame == null) {
      initComponents(myInfo, ppm);
    }
    frame.setVisible(true);
  }

  private String discoverMyIpAddressFromRemote() throws IOException {
    String theIp = null;
    HttpRequestWrapper r = new HttpRequestWrapper("http://www.vassalengine.org/util/getMyAddress");
    Enumeration e = r.doGet(null);
    if (e.hasMoreElements()) {
      theIp = (String) e.nextElement();
    }
    else {
      throw new IOException("Empty response");
    }
    return theIp;
  }

  public void disconnect() {
    if (frame != null) {
      frame.dispose();
      frame = null;
    }
    if (acceptThread != null) {
      acceptThread.halt();
      acceptThread = null;
    }
  }

  public void connectFailed(PeerInfo peerInfo) {
    JOptionPane.showMessageDialog(frame, "Could not reach " + peerInfo.getAddresses() + ":" + peerInfo.getPort(),
                                  "Invite failed", JOptionPane.INFORMATION_MESSAGE);
  }

  public void initComponents(final P2PPlayer me, final PendingPeerManager ppm) {
    Frame owner = null;
    if (GameModule.getGameModule() != null) {
      owner = GameModule.getGameModule().getFrame();
    }
    frame = new JDialog(owner,"Direct Connection");
    frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
    frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
    frame.getContentPane().add(createLabel("Your IP address is " + myIp + ":" + me.getInfo().getPort()));
    frame.getContentPane().add(createLabel("Enter another player's address below and hit the \"Invite\" button."));
    frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    Box b = Box.createHorizontalBox();
    b.setAlignmentX(0.0f);
    JButton invite = new JButton("Invite");
    b.add(invite);
    final JTextField tf = new JTextField("address:port");
    b.add(tf);
    frame.getContentPane().add(b);
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        PeerInfo info = PeerInfo.deFormat(tf.getText());
        if (info != null) {
          ppm.addNewPeer(info);
          tf.setText("");
        }
        else {
          JOptionPane.showMessageDialog(frame, "Invalid format <host>:<port>");
        }
      }
    };
    invite.addActionListener(al);
    tf.addActionListener(al);
    frame.pack();
    frame.setLocationRelativeTo(owner);
  }

  private JLabel createLabel(String text) {
    JLabel l = new JLabel(text);
    l.setAlignmentX(0.0f);
    return l;
  }
}