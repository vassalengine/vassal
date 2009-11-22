/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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
package VASSAL.chat.peer2peer;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.Enumeration;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import org.litesoft.p2pchat.PeerInfo;
import org.litesoft.p2pchat.PendingPeerManager;

import VASSAL.build.GameModule;
import VASSAL.chat.HttpRequestWrapper;
import VASSAL.chat.ui.ChatControlsInitializer;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.Dialogs;

/**
 * Date: Mar 12, 2003
 */
public class DirectPeerPool implements PeerPool, ChatControlsInitializer {
  protected static final String ADDRESS_PREF = "PeerAddressBook";
  private AcceptPeerThread acceptThread;
  private JButton inviteButton;
  private JDialog frame;
  private String myIp;
  private StringArrayConfigurer addressConfig;
  private JButton invitePeerButton;
  private JButton addButton;
  private JButton removeButton;
  private JList addressList; 
  private DefaultListModel addressBook;

  public DirectPeerPool() {
    inviteButton = new JButton(Resources.getString("Peer2Peer.invite_players")); //$NON-NLS-1$
    inviteButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        frame.setVisible(true);
      }
    });
    inviteButton.setEnabled(false);
  }

  public void initialize(P2PPlayer myInfo, PendingPeerManager ppm) throws IOException {
    myIp = myInfo.getInfo().getAddresses();
    try {
      myIp = discoverMyIpAddressFromRemote();
    }
    // FIXME: review error message
    catch (IOException e) {
    }
    acceptThread = new AcceptPeerThread(myInfo.getInfo().getPort(), ppm);
    acceptThread.setName("Accept Peer Thread"); //$NON-NLS-1$
    myInfo.getInfo().setPort(acceptThread.getPort());
    acceptThread.start();
    if (frame == null) {
      initComponents(myInfo, ppm);
      inviteButton.setEnabled(true);
    }
  }

  private String discoverMyIpAddressFromRemote() throws IOException {
    String theIp = null;
    HttpRequestWrapper r = new HttpRequestWrapper("http://www.vassalengine.org/util/getMyAddress"); //$NON-NLS-1$
    List<String> l = r.doGet(null);
    if (!l.isEmpty()) {
      theIp = l.get(0);
    }
    else {
      throw new IOException(Resources.getString("Server.empty_response")); //$NON-NLS-1$
    }
    return theIp;
  }

  public void disconnect() {
    if (frame != null) {
      frame.dispose();
      frame = null;
      inviteButton.setEnabled(false);
    }
    if (acceptThread != null) {
      acceptThread.halt();
      acceptThread = null;
    }
  }

  public void connectFailed(PeerInfo peerInfo) {
    JOptionPane.showMessageDialog(frame, Resources.getString("Peer2Peer.could_not_reach", peerInfo.getAddresses(), String.valueOf(peerInfo.getPort())), //$NON-NLS-1$
                                  Resources.getString("Peer2Peer.invite_failed"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$
  }

  public void initComponents(final P2PPlayer me, final PendingPeerManager ppm) {
    
    // Retrieve Address Book from preference 
    addressConfig = new StringArrayConfigurer(ADDRESS_PREF, null); 
    Prefs.getGlobalPrefs().addOption(null, addressConfig);
    String[] encodedEntries = addressConfig.getStringArray();
    addressBook = new DefaultListModel();
    addressList = new JList(addressBook);
    
    for (String e : encodedEntries) {
      addToList(new Entry(e));
    }
    
    Frame owner = null;
    if (GameModule.getGameModule() != null) {
      owner = GameModule.getGameModule().getFrame();
    }
    frame = new JDialog(owner,Resources.getString("Peer2Peer.direct_connection")); //$NON-NLS-1$
    frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);  
    frame.setLayout(new MigLayout());  
    final JScrollPane scroll = new JScrollPane(addressList);
    frame.add(scroll, "span 3, grow, push, w 300, h 400, wrap");

    invitePeerButton = new JButton("Invite");
    invitePeerButton.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent e) {
        invite(ppm);
      }});
    frame.add(invitePeerButton, "grow, push");
    
    addButton = new JButton("Add");
    addButton.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent e) {
        addEntry();
      }});
    frame.add(addButton, "grow, push");
    
    removeButton = new JButton("Remove");
    removeButton.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent arg0) {
        removeEntries();        
      }});
    frame.add(removeButton, "grow, push");
    
    
    frame.pack();
    frame.setLocationRelativeTo(owner);
    
    return;
    
  }

  protected void invite(final PendingPeerManager ppm) {
    final int selected[] = addressList.getSelectedIndices();
    for (int i = 0; i < selected.length; i++) {
      final Entry entry = (Entry) addressBook.getElementAt(selected[i]);
      final PeerInfo info = PeerInfo.deFormat(entry.getAddress()+":"+entry.getPort()+" "+entry.getDescription());
      if (info != null) {
        ppm.addNewPeer(info);
        GameModule.getGameModule().warn("Invitation sent to "+entry.toString());
      }
      else {
        JOptionPane.showMessageDialog(frame, Resources.getString("Peer2Peer.invalid_format")); //$NON-NLS-1$
      }
    }
  }
  
  protected void addEntry() {
    final JTextField description = new JTextField();
    final JTextField address = new JTextField();
    final JTextField port = new JTextField("5050");
    
    final JPanel addPanel = new JPanel(new MigLayout("", "[align right]rel[]", "")); //$NON-NLS-1$
    addPanel.add(new JLabel(Resources.getString("Editor.description_label"))); //$NON-NLS-1$
    addPanel.add(description, "wrap, grow, push"); //$NON-NLS-1$
    addPanel.add(new JLabel(Resources.getString("Chat.ip_address"))); //$NON-NLS-1$
    addPanel.add(address, "wrap, grow, push"); //$NON-NLS-1$
    addPanel.add(new JLabel(Resources.getString("ServerAddressBook.port"))); //$NON-NLS-1$
    addPanel.add(port, "wrap, grow, push"); //$NON-NLS-1$
    
    if (0 == (Integer) Dialogs.showDialog(null, "Add Peer to Peer connection",
        addPanel, JOptionPane.PLAIN_MESSAGE, null, JOptionPane.OK_CANCEL_OPTION,
        null, null, null, null)) {
        final Entry e = new Entry(description.getText(), address.getText(), port.getText());

        if (! addressBook.contains(e)) {
          addToList(e);
          saveAddressBook();
        }
    }
  }
  
  protected void addToList(Entry e) {
    boolean added = false;
    for (int i = 0; i < addressBook.getSize() && !added; i++) {
      if (e.compareTo((Entry) (addressBook.getElementAt(i))) < 0) {
        addressBook.insertElementAt(e, i);
        added = true;
      }
    }
    if (! added) {
      addressBook.addElement(e);
    }
  }
  
  protected void removeEntries() {
    final int selected[] = addressList.getSelectedIndices();
    if (selected.length == 0) {
      return;
    }
    final Entry entries[] = new Entry[selected.length];
    for (int i = 0; i < selected.length; i++) {
      entries[i] = (Entry) addressBook.getElementAt(selected[i]);
    }
    
    String mess = "<html>" 
       + (entries.length == 1 ? Resources.getString("Peer2Peer.remove_entry") : Resources.getString("Peer2Peer.remove_entries", entries.length)) 
       + "<br>";
    for (int i = 0; i < entries.length; i++) {
      mess += "&nbsp;" + entries[i].toString() + "<br>";
    }

    if (0 == (Integer) Dialogs.showDialog(null, "Remove Entries",
        new JLabel(mess), JOptionPane.PLAIN_MESSAGE, null, JOptionPane.OK_CANCEL_OPTION,
        null, null, null, null)) {     
    
      for (int i = 0; i < entries.length; i++) {
        addressBook.removeElement(entries[i]);
      }
    
      saveAddressBook();
    }
  }
  
  protected void saveAddressBook() {
    final String[] entries = new String[addressBook.size()];
    int i = 0;
    for (Enumeration<?> e = addressBook.elements(); e.hasMoreElements(); ) {
      entries[i++] = ((Entry) e.nextElement()).encode();
    }
    addressConfig.setValue(entries);
  }

  public void initializeControls(ChatServerControls controls) {
    controls.getToolbar().add(inviteButton);
  }

  public void uninitializeControls(ChatServerControls controls) {
    controls.getToolbar().remove(inviteButton);
    controls.getToolbar().repaint();
  }
  
  private class Entry implements Comparable<Entry>{
    String description;
    String address;
    String port;
    
    public Entry (String description, String address, String port) {
      this.description =description;
      this.address = address;
      this.port = port;
    }
    
    public Entry (String s) {
      decode(s);
    }
    
    public String getDescription() {
      return description;
    }
    
    public String getAddress() {
      return address;
    }
    
    public String getPort() {
      return port;
    }
    
    public String toString() {
      return description + " [" + address + ":" + port + "]";
    }
    
    private void decode(String s) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
      description = sd.nextToken("");
      address = sd.nextToken("");
      port = sd.nextToken("5050");
    }
    
    public String encode() {
      SequenceEncoder se = new SequenceEncoder('|');
      se.append(description);
      se.append(address);
      se.append(port);
      return se.getValue();      
    }

    public int compareTo(Entry e) {
      return toString().compareTo(e.toString());
    }
  }
}
