/*
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
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
import java.util.Properties;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import org.litesoft.p2pchat.PeerInfo;
import org.litesoft.p2pchat.PendingPeerManager;

import VASSAL.build.GameModule;
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
  protected static final String ADDRESS_PREF = "PeerAddressBook"; //$NON-NLS-1$
  private AcceptPeerThread acceptThread;
  private JButton inviteButton;
  private JDialog frame;
  private int listenPort;
  private static StringArrayConfigurer addressConfig;
  private JButton invitePeerButton;
  private JButton addButton;
  private JButton editButton;
  private JButton removeButton;
  private JList<Entry> addressList;
  private DefaultListModel<Entry> addressBook;
  private Properties params;
  private boolean serverMode;

  public DirectPeerPool() {
    this (new Properties());
  }

  public DirectPeerPool(Properties param) {
    params = new Properties();
    params.putAll(param);
    serverMode = P2PClientFactory.P2P_SERVER_MODE.equals(params.getProperty(P2PClientFactory.P2P_MODE_KEY));
    inviteButton = new JButton(Resources.getString("Peer2Peer.connect")); //$NON-NLS-1$
    inviteButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        frame.setVisible(true);
      }
    });
    inviteButton.setEnabled(false);
    inviteButton.setVisible(P2PClientFactory.P2P_SERVER_MODE.equals(params.getProperty(P2PClientFactory.P2P_MODE_KEY)));
  }

  @Override
  public void initialize(P2PPlayer myInfo, PendingPeerManager ppm) throws IOException {
    listenPort = 5050;
    String port = params.getProperty(P2PClientFactory.P2P_LISTEN_PORT);
    if (port != null && port.length() > 0) {
      try {
        listenPort = Integer.parseInt(port);
      }
      catch (NumberFormatException e) {
        // No error;
      }
    }

    acceptThread = new AcceptPeerThread(listenPort, ppm);
    acceptThread.setName("Accept Peer Thread"); //$NON-NLS-1$
    myInfo.getInfo().setPort(acceptThread.getPort());
    acceptThread.start();
    if (frame == null) {
      initComponents(myInfo, ppm);
      inviteButton.setEnabled(true);
    }
  }

  protected boolean isServerMode() {
    return serverMode;
  }

  @Override
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

  @Override
  public void connectFailed(PeerInfo peerInfo) {
    JOptionPane.showMessageDialog(frame, Resources.getString("Peer2Peer.could_not_reach", peerInfo.getAddresses(), String.valueOf(peerInfo.getPort())), //$NON-NLS-1$
                                  Resources.getString("Peer2Peer.invite_failed"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$
  }

  public void initComponents(final P2PPlayer me, final PendingPeerManager ppm) {

    // Retrieve Address Book from preference
    if (addressConfig == null) {
      addressConfig = new StringArrayConfigurer(ADDRESS_PREF, null);
      Prefs.getGlobalPrefs().addOption(null, addressConfig);
    }
    String[] encodedEntries = addressConfig.getStringArray();
    addressBook = new DefaultListModel<>();
    addressList = new JList<>(addressBook);

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

    frame.add(new WTextArea(Resources.getString("Peer2Peer.other_players_address")), "growx, span 4,wrap"); //$NON-NLS-1$ //$NON-NLS-2$

    final JScrollPane scroll = new JScrollPane(addressList);
    frame.add(scroll, "span 4, grow, push, w 300, h 400, wrap"); //$NON-NLS-1$

    invitePeerButton = new JButton(Resources.getString("Peer2Peer.connect")); //$NON-NLS-1$
    invitePeerButton.setToolTipText(Resources.getString("Peer2Peer.invite_button_tooltip")); //$NON-NLS-1$))
    invitePeerButton.addActionListener(new ActionListener(){
      @Override
      public void actionPerformed(ActionEvent e) {
        invite(ppm);
      }});
    frame.add(invitePeerButton, "growx, push"); //$NON-NLS-1$

    addButton = new JButton(Resources.getString("General.add")); //$NON-NLS-1$
    addButton.setToolTipText(Resources.getString("Peer2Peer.add_button_tooltip")); //$NON-NLS-1$))
    addButton.addActionListener(new ActionListener(){
      @Override
      public void actionPerformed(ActionEvent e) {
        addEntry();
      }});
    frame.add(addButton, "growx, push"); //$NON-NLS-1$

    editButton = new JButton(Resources.getString("General.edit")); //$NON-NLS-1$
    editButton.setToolTipText(Resources.getString("Peer2Peer.edit_button_tooltip")); //$NON-NLS-1$))
    editButton.addActionListener(new ActionListener(){
      @Override
      public void actionPerformed(ActionEvent e) {
        editEntry();
      }});
    frame.add(editButton, "growx, push"); //$NON-NLS-1$

    removeButton = new JButton(Resources.getString("General.remove")); //$NON-NLS-1$
    removeButton.setToolTipText(Resources.getString("Peer2Peer.remove_button_tooltip")); //$NON-NLS-1$))
    removeButton.addActionListener(new ActionListener(){
      @Override
      public void actionPerformed(ActionEvent arg0) {
        removeEntries();
      }});
    frame.add(removeButton, "growx, push, wrap"); //$NON-NLS-1$


    frame.pack();
    frame.setLocationRelativeTo(owner);

    return;

  }

  protected void invite(final PendingPeerManager ppm) {
    final int[] selected = addressList.getSelectedIndices();
    for (int value : selected) {
      final Entry entry = addressBook.getElementAt(value);
      final PeerInfo info = PeerInfo.deFormat(entry.getAddress() + ":" + entry.getPort() + " " + entry.getDescription()); //$NON-NLS-1$ //$NON-NLS-2$
      if (info != null) {
        ppm.addNewPeer(info);
        GameModule.getGameModule().warn(Resources.getString("Chat.invite_sent", entry.toString())); //$NON-NLS-1$
      }
      else {
        JOptionPane.showMessageDialog(frame, Resources.getString("Peer2Peer.invalid_format")); //$NON-NLS-1$
      }
    }
  }

  protected void addEntry() {
    final Entry e = new Entry();
    if (e.edit()) {
      if (!addressBook.contains(e)) {
        addToList(e);
        saveAddressBook();
      }
    }
  }

  protected void addToList(Entry e) {
    boolean added = false;
    for (int i = 0; i < addressBook.getSize() && !added; i++) {
      if (e.compareTo(addressBook.getElementAt(i)) < 0) {
        addressBook.insertElementAt(e, i);
        added = true;
      }
    }
    if (! added) {
      addressBook.addElement(e);
    }
  }

  protected void editEntry() {
    final int index = addressList.getSelectedIndex();
    if (index >= 0) {
      final Entry e = addressBook.getElementAt(index);
      if (e.edit()) {
        addressBook.removeElementAt(index);
        addToList(e);
        saveAddressBook();
      }
    }
  }

  protected void removeEntries() {
    final int[] selected = addressList.getSelectedIndices();
    if (selected.length == 0) {
      return;
    }
    final Entry[] entries = new Entry[selected.length];
    for (int i = 0; i < selected.length; i++) {
      entries[i] = addressBook.getElementAt(selected[i]);
    }

    final JPanel queryPanel = new JPanel(new MigLayout("", "10[][]10"));
    final String mess = (entries.length == 1 ? Resources.getString("Peer2Peer.remove_entry") : Resources.getString("Peer2Peer.remove_entries", entries.length));  //$NON-NLS-1$ //$NON-NLS-2$
    queryPanel.add(new JLabel(mess), "align center, wrap");
    queryPanel.add(new JLabel(), "wrap");
    for (Entry entry : entries) {
      queryPanel.add(new JLabel(entry.toString()), "wrap");
    }

    final Integer result = (Integer) Dialogs.showDialog(null, Resources.getString("Peer2Peer.remove_entry"), //$NON-NLS-1$
        queryPanel, JOptionPane.QUESTION_MESSAGE, null, JOptionPane.OK_CANCEL_OPTION,
        null, null, null, null);
    if (result != null && result == 0) {
      for (Entry entry : entries) {
        addressBook.removeElement(entry);
      }

      saveAddressBook();
    }
  }

  protected void saveAddressBook() {
    final String[] entries = new String[addressBook.size()];
    int i = 0;
    for (Enumeration<Entry> e = addressBook.elements(); e.hasMoreElements(); ) {
      entries[i++] = e.nextElement().encode();
    }
    addressConfig.setValue(entries);
  }

  @Override
  public void initializeControls(ChatServerControls controls) {
    controls.getToolbar().add(inviteButton);
  }

  @Override
  public void uninitializeControls(ChatServerControls controls) {
    controls.getToolbar().remove(inviteButton);
    controls.getToolbar().repaint();
  }

  /**
   * A class representing the address of another player's computer.
   *
   */
  private class Entry implements Comparable<Entry>{
    String description;
    String address;
    String port;
    JTextField descriptionField;
    JTextField addressField;
    JTextField portField;

    public Entry () {
      this("", "", "5050", "");
    }

    public Entry (String description, String address, String port, String passwd) {
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

//    public String getPasswd() {
//      return passwd;
//    }

    public String toString() {
      return description + " [" + address + ":" + port; // + (getPasswd().length() == 0 ? "" : "/") +  getPasswd() + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }

    private void decode(String s) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
      description = sd.nextToken(""); //$NON-NLS-1$
      address = sd.nextToken(""); //$NON-NLS-1$
      port = sd.nextToken("5050"); //$NON-NLS-1$
    }

    public String encode() {
      SequenceEncoder se = new SequenceEncoder('|');
      se.append(description);
      se.append(address);
      se.append(port);
      return se.getValue();
    }

    @Override
    public int compareTo(Entry e) {
      return toString().compareTo(e.toString());
    }

    public boolean edit() {
      descriptionField = new JTextField(description);
      addressField = new JTextField(address);
      portField = new JTextField(port);

      final JPanel editPanel = new JPanel(new MigLayout("", "[align right]rel[]", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      editPanel.add(new JLabel(Resources.getString("Editor.description_label"))); //$NON-NLS-1$
      editPanel.add(descriptionField, "wrap, grow, push"); //$NON-NLS-1$
      editPanel.add(new JLabel(Resources.getString("Chat.ip_address"))); //$NON-NLS-1$
      editPanel.add(addressField, "wrap, grow, push"); //$NON-NLS-1$
      editPanel.add(new JLabel(Resources.getString("ServerAddressBook.port"))); //$NON-NLS-1$
      editPanel.add(portField, "wrap, grow, push"); //$NON-NLS-1$

      final Integer result = (Integer) Dialogs.showDialog(null, Resources.getString("Peer2Peer.add_peer_connection"), //$NON-NLS-1$
          editPanel, JOptionPane.PLAIN_MESSAGE, null, JOptionPane.OK_CANCEL_OPTION,
          null, null, null, null);

      if (result != null && result == 0) {
        description = descriptionField.getText();
        address = addressField.getText();
        port = portField.getText();
        return true;
      }

      return false;
    }
  }

  private class WTextArea extends JTextArea {
    private static final long serialVersionUID = 1L;
    public WTextArea(String s) {
      super(s);
      setEditable(false);
      setLineWrap(true);
      setWrapStyleWord(true);
      setBackground(UIManager.getColor("OptionPane.background"));
    }
  }
}
