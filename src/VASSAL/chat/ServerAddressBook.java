/* 
 * $Id: ServerAddressBook.java 4997 2009-01-31 05:00:33Z rodneykinney $
 *
 * Copyright (c) 2009 by Brent Easton
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
package VASSAL.chat;

import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.miginfocom.swing.MigLayout;
import VASSAL.build.GameModule;
import VASSAL.chat.jabber.JabberClientFactory;
import VASSAL.chat.node.NodeClientFactory;
import VASSAL.chat.peer2peer.P2PClientFactory;
import VASSAL.configure.StringConfigurer;
import VASSAL.preferences.Prefs;
import VASSAL.tools.PropertiesEncoder;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
import VASSAL.tools.swing.Dialogs;

public class ServerAddressBook {
  public static final String CURRENT_SERVER = "currentServer";
  protected static final String ADDRESS_PREF = "ServerAddressBook";
  protected static final String LEGACY_TYPE = NodeClientFactory.NODE_TYPE;
  protected static final String DYNAMIC_TYPE = DynamicClientFactory.DYNAMIC_TYPE;
  protected static final String JABBER_TYPE = JabberClientFactory.JABBER_SERVER_TYPE;
  protected static final String P2P_TYPE = P2PClientFactory.P2P_TYPE;
  protected static final String TYPE_KEY = ChatServerFactory.TYPE_KEY;
  protected static final String DESCRIPTION_KEY = "description";
  protected final int LEAF_ICON_SIZE = IconFamily.SMALL;
  protected boolean frozen;

  private JPanel controls;
  private StringConfigurer addressConfig;
  private JList myList;
  private DefaultListModel addressBook;
  private AddressBookEntry currentEntry;
  private boolean enabled = true;
  protected PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);

  private JButton addButton;
  private JButton removeButton;
  private JButton editButton;
  private JButton setButton;

  public JComponent getControls() {
    if (controls == null) {

      controls = new JPanel(new MigLayout());
      addressConfig = new StringConfigurer(ADDRESS_PREF, null, "");
      Prefs.getGlobalPrefs().addOption(null, addressConfig);
      addressBook = new DefaultListModel();
      loadAddressBook();
      myList = new JList(addressBook);
      myList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      myList.setCellRenderer(new MyRenderer());
      myList.addListSelectionListener(new ListSelectionListener() {
        public void valueChanged(ListSelectionEvent evt) {
          updateButtonVisibility();
        }
      });
      final JScrollPane scroll = new JScrollPane(myList);
      controls.add(scroll, "grow, push, w 500, h 400, wrap, span 4");

      addButton = new JButton("Add");
      addButton.setToolTipText("Add a new Server");
      addButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          addServer();
        }
      });

      removeButton = new JButton("Remove");
      removeButton.setToolTipText("Remove selected Server");
      removeButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          removeServer(myList.getSelectedIndex());
        }
      });

      editButton = new JButton("Edit");
      editButton.setToolTipText("Edit selected Server");
      editButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          editServer(myList.getSelectedIndex());
        }
      });

      setButton = new JButton("Set Current");
      setButton.setToolTipText("Set selected Server as current Server");
      setButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setCurrentServer(myList.getSelectedIndex());
        }
      });

      controls.add(addButton, "grow, push");
      controls.add(editButton, "grow, push");
      controls.add(removeButton, "grow, push");
      controls.add(setButton, "grow, push");

      updateButtonVisibility();
    }
    return controls;
  }

  public void setEnabled(boolean b) {
    enabled = b;
    updateButtonVisibility();
  }

  public boolean isEnabled() {
    return enabled;
  }
  
  public void setFrozen(boolean b) {
    frozen = b;
  }

  private void updateButtonVisibility() {
    final int index = myList.getSelectedIndex();
    if (index >= 0) {
      final AddressBookEntry e = (AddressBookEntry) addressBook.get(index);
      editButton.setEnabled(e.isRemovable() && (isEnabled() || !e.isCurrent()));
      removeButton.setEnabled(e.isRemovable() && !e.isCurrent());
      setButton.setEnabled(isEnabled() && !e.isCurrent());
    }
    else {
      editButton.setEnabled(false);
      removeButton.setEnabled(false);
      setButton.setEnabled(false);
    }
  }

  public void setCurrentServer(Properties p) {

    // Check for Dynamic Types, regardless of other properties
    int index = 0;
    final String type = p.getProperty(TYPE_KEY);
    final String dtype = p.getProperty(DYNAMIC_TYPE);
    for (Enumeration<?> e = addressBook.elements(); e.hasMoreElements();) {
      final AddressBookEntry entry = (AddressBookEntry) e.nextElement();
      final Properties ep = entry.getProperties();

      if (ep.equals(p)) {
        setCurrentServer(index);
        return;
      }

      else if (DYNAMIC_TYPE.equals(type)
          && DYNAMIC_TYPE.equals(ep.getProperty(TYPE_KEY))
          && ep.getProperty(DYNAMIC_TYPE).equals(dtype)) {
        setCurrentServer(index);
        return;
      }

      index++;
    }

    // Some Server we don't know about, add a server entry
    final AddressBookEntry newEntry = buildEntry(p);
    addressBook.addElement(newEntry);
    setCurrentServer(addressBook.indexOf(newEntry));
    saveAddressBook();

  }

  private void setCurrentServer(int index) {
    final AddressBookEntry e = (AddressBookEntry) addressBook.get(index);
    if (currentEntry != null) {
      currentEntry.setCurrent(false);
    }
    final Properties oldProps = currentEntry == null ? null : currentEntry.getProperties();
    currentEntry = e;
    currentEntry.setCurrent(true);    
    if (! frozen) {
      changeSupport.firePropertyChange(CURRENT_SERVER, oldProps, e.getProperties());     
    }
    updateButtonVisibility();
    myList.repaint();
  }

  public void addPropertyChangeListener(PropertyChangeListener l) {
    changeSupport.addPropertyChangeListener(l);
  }
  
  public void removePropertyChangeListener(PropertyChangeListener l) {
    changeSupport.removePropertyChangeListener(l);
  }
  
  private void editServer(int index) {
    final AddressBookEntry e = (AddressBookEntry) addressBook.get(index);
    final ServerConfig config = new ServerConfig(e.getProperties());
    if (0 == (Integer) Dialogs.showDialog(null, "Edit Server Configuration",
        config, JOptionPane.PLAIN_MESSAGE, null, JOptionPane.OK_CANCEL_OPTION,
        null, null, null, null)) {
      e.setProperties(config.getProperties());
      saveAddressBook();
    }
  }

  private void removeServer(int index) {
    final AddressBookEntry e = (AddressBookEntry) addressBook.get(index);
    int i = JOptionPane.showConfirmDialog(
        GameModule.getGameModule().getFrame(), "Remove Server "
            + e.getDescription() + "?");
    if (i == 0) {
      addressBook.remove(index);
      myList.setSelectedIndex(-1);
      myList.repaint();
      updateButtonVisibility();
      saveAddressBook();
    }

  }

  private void addServer() {
    final ServerConfig config = new ServerConfig();
    if (0 == (Integer) Dialogs.showDialog(null, "New Server Configuration",
        config, JOptionPane.PLAIN_MESSAGE, null, JOptionPane.OK_CANCEL_OPTION,
        null, null, null, null)) {
      final AddressBookEntry e = buildEntry(config.getProperties());
      addressBook.addElement(e);
      saveAddressBook();
    }
  }

  public Properties getDefaultServerProperties() {
    return (new VassalJabberEntry()).getProperties();
  }

  private void loadAddressBook() {
    decodeAddressBook(addressConfig.getValueString());
    // Ensure that the Address Book has the VASSAL legacy and VASSAL Jabber
    // servers in it.
    boolean legacy = false;
    boolean jabber = false;
    for (Enumeration<?> e = addressBook.elements(); e.hasMoreElements();) {
      final AddressBookEntry entry = (AddressBookEntry) e.nextElement();
      if (entry instanceof LegacyEntry) {
        legacy = true;
      }
      else if (entry instanceof VassalJabberEntry) {
        jabber = true;
      }
    }
    if (!legacy) {
      addressBook.addElement(new LegacyEntry());
      saveAddressBook();
    }
    if (!jabber) {
      final AddressBookEntry entry = new VassalJabberEntry();
      entry.setCurrent(true);
      currentEntry = entry;
      addressBook.addElement(entry);
      saveAddressBook();
    }

  }

  private void saveAddressBook() {
    addressConfig.setValue(encodeAddressBook());
    if (myList != null) {
      myList.repaint();
    }
  }

  private String encodeAddressBook() {
    SequenceEncoder se = new SequenceEncoder(',');
    for (Enumeration<?> e = addressBook.elements(); e.hasMoreElements();) {
      final AddressBookEntry entry = (AddressBookEntry) e.nextElement();
      se.append(entry.encode());
    }
    return se.getValue();
  }

  private void decodeAddressBook(String s) {
    addressBook.clear();
    for (SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ','); sd
        .hasMoreTokens();) {
      final String token = sd.nextToken("");
      if (token.length() > 0) {
        addressBook.addElement(buildEntry(token));
      }
    }
  }

  /**
   * Return an appropriately typed Entry, depending on the Server Properties
   * passed
   * 
   * @param s
   *          Encoded Server Properties
   * @return Entry
   */
  private AddressBookEntry buildEntry(String s) {
    Properties newProperties = new Properties();
    try {
      newProperties = new PropertiesEncoder(s).getProperties();
    }
    catch (IOException e) {
      // FIXME: Error Message?
    }
    return buildEntry(newProperties);
  }

  private AddressBookEntry buildEntry(Properties newProperties) {
    final String type = newProperties.getProperty(TYPE_KEY);
    if (JABBER_TYPE.equals(type)) {
      return new JabberEntry(newProperties);
    }
    else if (DYNAMIC_TYPE.equals(type)) {
      final String dtype = newProperties.getProperty(DYNAMIC_TYPE);
      if (JABBER_TYPE.equals(dtype)) {
        return new VassalJabberEntry(newProperties);
      }
      else if (LEGACY_TYPE.equals(dtype)) {
        return new LegacyEntry(newProperties);
      }
    }
    else if (P2P_TYPE.equals(type)) {
      return new PeerEntry(newProperties);
    }
    return null;
  }

  /**
   * Base class for an Address Book Entry
   * 
   */
  abstract class AddressBookEntry implements Comparable<AddressBookEntry> {
    protected Properties properties = new Properties();
    protected boolean current;

    protected AddressBookEntry() {
      this(new Properties());
    }

    protected AddressBookEntry(Properties props) {
      properties = props;
    }

    protected String getDescription() {
      return getProperty(DESCRIPTION_KEY);
    }

    protected void setDescription(String desc) {
      properties.setProperty(DESCRIPTION_KEY, desc);
    }

    public String getProperty(String key) {
      return properties.getProperty(key);
    }

    public void setProperty(String key, String value) {
      properties.setProperty(key, value);
    }

    protected boolean isRemovable() {
      return true;
    }

    protected abstract Icon getIcon();

    public String getType() {
      return properties.getProperty(TYPE_KEY);
    }

    public void setType(String t) {
      properties.setProperty(TYPE_KEY, t);
    }

    public Properties getProperties() {
      return properties;
    }

    public void setProperties(Properties p) {
      properties = p;
    }

    public String encode() {
      return new PropertiesEncoder(properties).getStringValue();
    }

    public int compareTo(AddressBookEntry target) {
      if (getType().equals(target.getType())) {
        return getDescription().compareTo(target.getDescription());
      }
      return getType().compareTo(target.getType());
    }

    public boolean isCurrent() {
      return current;
    }

    public void setCurrent(boolean b) {
      current = b;
    }
  }

  /**
   * Address Book entry for a user defined Jabber Server
   * 
   */
  class JabberEntry extends AddressBookEntry {

    public JabberEntry(String description) {
      super();
      setProperty(DESCRIPTION_KEY, description);
    }

    public JabberEntry(Properties props) {
      super(props);
    }

    public String toString() {
      return getDescription() + " - "
          + getProperty(JabberClientFactory.JABBER_HOST) + ":"
          + getProperty(JabberClientFactory.JABBER_PORT) + " "
          + getProperty(JabberClientFactory.JABBER_LOGIN) + "/"
          + getProperty(JabberClientFactory.JABBER_PWD);
    }

    protected Icon getIcon() {
      return IconFactory.getIcon("jabber", LEAF_ICON_SIZE);
    }

  }

  /**
   * Address Book entry for the VASSAL Jabber server
   * 
   */
  class VassalJabberEntry extends JabberEntry {

    public VassalJabberEntry() {
      this(new Properties());
      setDescription("VASSAL Jabber/XMPP Server");
      setType(DYNAMIC_TYPE);
      setProperty(DynamicClientFactory.DYNAMIC_TYPE,
          JabberClientFactory.JABBER_SERVER_TYPE);
    }

    public VassalJabberEntry(Properties props) {
      super(props);
    }

    public String toString() {
      return getDescription();
    }

    protected boolean isRemovable() {
      return false;
    }

  }

  /**
   * Address Book entry for the VASSAL legacy server
   * 
   */
  class LegacyEntry extends AddressBookEntry {

    public LegacyEntry() {
      this(new Properties());
      setDescription("VASSAL Legacy Server");
      setType(DYNAMIC_TYPE);
      setProperty(DynamicClientFactory.DYNAMIC_TYPE,
          NodeClientFactory.NODE_TYPE);
    }

    public LegacyEntry(Properties props) {
      super(props);
    }

    public String toString() {
      return getDescription();
    }

    protected Icon getIcon() {
      return IconFactory.getIcon("VASSAL", LEAF_ICON_SIZE);
    }

    protected boolean isRemovable() {
      return false;
    }

  }

  /**
   * Address Book Entry for a Peer to Peer connection
   * 
   */
  class PeerEntry extends AddressBookEntry {

    public PeerEntry(Properties props) {
      super(props);
    }

    public String toString() {
      return getDescription() + " - "
          + getProperty(JabberClientFactory.JABBER_HOST) + ":"
          + getProperty(JabberClientFactory.JABBER_PORT);
    }

    protected Icon getIcon() {
      return IconFactory.getIcon("network-idle", LEAF_ICON_SIZE);
    }
  }

  /**
   * Customised List Cell Renderer for the JList display: - Display the Icon
   * appropriate to the Server Entry - Highlight the currently selected server
   * 
   */
  class MyRenderer extends DefaultListCellRenderer {
    private static final long serialVersionUID = 1L;
    private Font standardFont;
    private Font highlightFont;

    public Component getListCellRendererComponent(JList list, Object value,
        int index, boolean isSelected, boolean cellHasFocus) {

      super.getListCellRendererComponent(list, value, index, isSelected,
          cellHasFocus);

      if (standardFont == null) {
        standardFont = getFont();
        highlightFont = new Font(standardFont.getFamily(), Font.BOLD
            + Font.ITALIC, standardFont.getSize());
      }

      if (value instanceof AddressBookEntry) {
        final AddressBookEntry e = (AddressBookEntry) value;
        setIcon(e.getIcon());
        if (e.isCurrent()) {
          setFont(highlightFont);
          setText(e.toString() + " (Current)");
        }
        else {
          setFont(standardFont);
        }

      }
      return this;
    }
  }

  /**
   * Class to Edit a Server Configuration
   * 
   */
  class ServerConfig extends JPanel {
    private static final long serialVersionUID = 1L;
    private JTextField description;
    private JRadioButton jabberButton;
    private JRadioButton peerButton;
    private JTextField jabberHost;
    private JTextField jabberPort;
    private JTextField jabberUser;
    private JTextField jabberPw;
    private JTextField peerHost;
    private JTextField peerPort;
    private JPanel detailPanel;
    private CardLayout detailLayout;
    private JPanel jabberPanel;
    private JPanel peerPanel;

    public ServerConfig() {
      setLayout(new MigLayout("", "[align right]rel[]", ""));
      description = new JTextField();
      add(new JLabel("Description:"));
      add(description, "wrap, grow, push");

      jabberButton = new JRadioButton();
      jabberButton.setSelected(true);
      jabberButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          peerButton.setSelected(!jabberButton.isSelected());
          detailLayout.show(detailPanel, JABBER_TYPE);
        }
      });
      add(jabberButton);
      add(new JLabel("New Jabber/XMPP Server"), "wrap, grow, push");

      peerButton = new JRadioButton();
      peerButton.setSelected(false);
      peerButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          jabberButton.setSelected(!peerButton.isSelected());
          detailLayout.show(detailPanel, P2P_TYPE);
        }
      });
      add(peerButton);
      add(new JLabel("New Peer to Peer Connection"), "wrap, grow, push");

      detailLayout = new CardLayout();
      detailPanel = new JPanel(detailLayout);
      detailPanel.setBorder(BorderFactory.createLineBorder(Color.lightGray));

      jabberHost = new JTextField();
      jabberPort = new JTextField("5222");
      jabberUser = new JTextField();
      jabberPw = new JTextField();
      jabberPanel = new JPanel(new MigLayout("", "[align right]rel[]", ""));
      jabberPanel.add(new JLabel("Jabber Host:"));
      jabberPanel.add(jabberHost, "wrap, grow, push");
      jabberPanel.add(new JLabel("Port:"));
      jabberPanel.add(jabberPort, "wrap, grow, push");
      jabberPanel.add(new JLabel("User Name:"));
      jabberPanel.add(jabberUser, "wrap, grow, push");
      jabberPanel.add(new JLabel("Password:"));
      jabberPanel.add(jabberPw, "wrap, grow, push");
      detailPanel.add(jabberPanel, JABBER_TYPE);

      peerHost = new JTextField();
      peerPort = new JTextField("5050");
      peerPanel = new JPanel(new MigLayout("", "[align right]rel[]", ""));
      peerPanel.add(new JLabel("Peer Address:"));
      peerPanel.add(peerHost, "wrap, growx, push");
      peerPanel.add(new JLabel("Port:"));
      peerPanel.add(peerPort, "wrap, growx, push");
      detailPanel.add(peerPanel, P2P_TYPE);

      add(detailPanel, "span 2, grow, push, wrap");
    }

    public ServerConfig(Properties props) {
      this();
      description.setText(props.getProperty(DESCRIPTION_KEY));
      if (JABBER_TYPE.equals(props.getProperty(TYPE_KEY))) {
        jabberButton.setSelected(true);
        jabberHost.setText(props.getProperty(JabberClientFactory.JABBER_HOST));
        jabberPort.setText(props.getProperty(JabberClientFactory.JABBER_PORT));
        jabberUser.setText(props.getProperty(JabberClientFactory.JABBER_LOGIN));
        jabberPw.setText(props.getProperty(JabberClientFactory.JABBER_PWD));
      }
      else {
        jabberButton.setSelected(false);
        peerHost.setText(props.getProperty(JabberClientFactory.JABBER_HOST));
        peerPort.setText(props.getProperty(JabberClientFactory.JABBER_PORT));
      }
    }

    public Properties getProperties() {
      if (description.getText().length() == 0) {
        return null;
      }
      final Properties props = new Properties();

      props.setProperty(DESCRIPTION_KEY, description.getText());
      if (jabberButton.isSelected()) {
        props.setProperty(TYPE_KEY, JABBER_TYPE);
        props
            .setProperty(JabberClientFactory.JABBER_HOST, jabberHost.getText());
        props
            .setProperty(JabberClientFactory.JABBER_PORT, jabberPort.getText());
        props.setProperty(JabberClientFactory.JABBER_LOGIN, jabberUser
            .getText());
        props.setProperty(JabberClientFactory.JABBER_PWD, jabberPw.getText());
      }
      else {
        props.setProperty(TYPE_KEY, P2P_TYPE);
        props.setProperty(JabberClientFactory.JABBER_HOST, peerHost.getText());
        props.setProperty(JabberClientFactory.JABBER_PORT, peerPort.getText());
      }
      return props;
    }

  }
}