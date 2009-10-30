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
package VASSAL.chat.jabber;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.RoomInfo;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.chat.LockableRoom;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.SimpleStatus;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.PropertiesEncoder;
import VASSAL.tools.swing.Dialogs;

public class JabberRoom extends SimpleRoom implements LockableRoom {
  private static final String JABBER_MEMBERSONLY = "muc#roomconfig_membersonly";  //$NON-NLS-1$
  private static final String JABBER_ALLOW_INVITES = "muc#roomconfig_allowinvites"; //$NON-NLS-1$
  private static final String JABBER_CHANGE_SUBJECT = "muc#roomconfig_changesubject"; //$NON-NLS-1$
  private static final String JABBER_MODERATED = "muc#roomconfig_moderatedroom"; //$NON-NLS-1$
  private static final String JABBER_PASSWORD_PROTECTED = "muc#roomconfig_passwordprotectedroom"; //$NON-NLS-1$
  private static final String JABBER_PERSISTENT = "muc#roomconfig_persistentroom"; //$NON-NLS-1$
  
  public static final String CONFIG_NAME = "name"; //$NON-NLS-1$
  public static final String CONFIG_LOCKED = "locked"; //$NON-NLS-1$
  public static final String CONFIG_VASSAL_VERSION = "vasVer"; //$NON-NLS-1$
  public static final String CONFIG_MIN_VASSAL_VERSION = "minVasVer"; //$NON-NLS-1$
  public static final String CONFIG_MODULE_VERSION = "modVer"; //$NON-NLS-1$
  public static final String CONFIG_MIN_MODULE_VERSION = "minModVer"; //$NON-NLS-1$
  public static final String CONFIG_CRC_CHECK = "crcCheck"; //$NON-NLS-1$
  public static final String CONFIG_CRC = "crc"; //$NON-NLS-1$
  
  private String jid;
  private RoomInfo info;
  private boolean ownedByMe;
  private JabberClient client;
  private ArrayList<String> owners = new ArrayList<String>();
  private Properties config = new Properties();

  private JabberRoom(String name, String jid, RoomInfo info, JabberClient client) {
    super(name);
    this.jid = jid;
    this.info = info;
    this.client = client;
    config.clear();
    config.put(CONFIG_NAME, name);
  }

  public static String jidToName(String jid) {
    final String roomRef = jid.split("@")[0]; //$NON-NLS-1$
    final String[] parts = roomRef.split("2f"); //$NON-NLS-1$
    return parts[parts.length-1];  
  }
  
  public String getJID() {
    return jid;
  }

  public boolean isLocked() {
    return info != null && info.isMembersOnly();
  }

  public void setInfo(RoomInfo info) {
    this.info = info;
    decodeConfig(info.getSubject());
  }
  
  public void toggleLock(MultiUserChat muc) {
    try {
      if (!isLocked()) {
        lock(muc);
      }
      else {
        unlock(muc);
      }
      info = MultiUserChat.getRoomInfo(client.getConnection(), jid);
    }
    catch (XMPPException e) {
      e.printStackTrace();
      return;
    }
  }

  protected void lock(MultiUserChat muc) throws XMPPException {
    final Form form = muc.getConfigurationForm().createAnswerForm();
    form.setAnswer(JABBER_MEMBERSONLY, true);
    muc.sendConfigurationForm(form);
  }

  protected void unlock(MultiUserChat muc) throws XMPPException {
    final Form form = muc.getConfigurationForm().createAnswerForm();
    form.setAnswer(JABBER_MEMBERSONLY, false);
    muc.sendConfigurationForm(form);
  }
  
  public MultiUserChat join(JabberClient client, JabberPlayer me)
      throws XMPPException {
    
    MultiUserChat chat = new MultiUserChat(client.getConnection(), getJID());
    chat.join(StringUtils.parseName(me.getJid()));

    if (!chat.isJoined()) {
      return null;
    }
    

    try {
      // This is necessary to create the room if it doesn't already exist
      // Configure the options we needs explicitly, don't depend on the server supplied defaults
      final Form configForm = chat.getConfigurationForm().createAnswerForm();
      configForm.setAnswer(JABBER_MEMBERSONLY, isStartLocked());
      configForm.setAnswer(JABBER_ALLOW_INVITES, false);
      configForm.setAnswer(JABBER_CHANGE_SUBJECT, false);
      configForm.setAnswer(JABBER_MODERATED, false);
      configForm.setAnswer(JABBER_PASSWORD_PROTECTED, false);
      configForm.setAnswer(JABBER_PERSISTENT, false);
      chat.changeSubject(encodeConfig());
      chat.sendConfigurationForm(configForm);
      ownedByMe = true;

    }
    catch (XMPPException e) {
      // 403 code means the room already exists and user is not an owner
      // Anything else is a problem.
      if (e.getXMPPError() != null && e.getXMPPError().getCode() != 403) {
        throw e;
      }
    }

    chat.addMessageListener(client);
    return chat;
  }
 
  public boolean equals(Object o) {
    if (o instanceof JabberRoom) {
      JabberRoom r = (JabberRoom) o;
      return r.jid.equals(jid);
    }
    else {
      return false;
    }
  }

  public int hashCode() {
    return jid.hashCode();
  }

  public boolean isOwnedByMe() {
    return ownedByMe;
  }
  
  public boolean isOwner(String jid) {
    return owners.contains(jid);
  }

  public void addOwner(String jid) {
    if (! owners.contains(jid)) {
      owners.add(jid);
    }
  }
  
  public void removeOwner(String jid) {
    owners.remove(jid);
  }
  
  public void setConfig(Properties props) {
    config = props;
  }
  
  public String encodeConfig() {
    final String s = new PropertiesEncoder(config).getStringValue();
    return s == null ? "" : s; //$NON-NLS-1$
  }
  
  public void decodeConfig(String s) {
    try {
      config = new PropertiesEncoder(s).getProperties();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }
  
  public boolean isStartLocked() {
    return "true".equals(config.getProperty(CONFIG_LOCKED)); //$NON-NLS-1$
  }
  
  public boolean isMatchCrc() {
    return "true".equals(config.getProperty(CONFIG_CRC_CHECK)); //$NON-NLS-1$
  }
  
  public String getCheckCrc() {
    return config.getProperty(CONFIG_CRC);
  }
  
  public String getVassalOption() {
    return config.getProperty(CONFIG_VASSAL_VERSION, ANY_OPTION);
  }
  
  public String getVassalVersion() {
    return config.getProperty(CONFIG_MIN_VASSAL_VERSION, Info.getVersion());
  }
  
  public String getModuleOption() {
    return config.getProperty(CONFIG_MODULE_VERSION, ANY_OPTION);
  }
  
  public String getModuleVersion() {
    return config.getProperty(CONFIG_MIN_MODULE_VERSION, GameModule.getGameModule().getGameVersion());
  }
  
  public void showConfig() {
    final JabberRoomConfig c = new JabberRoomConfig(config, false);    
    Dialogs.showDialog(null, Resources.getString("Chat.room_configuration"), c, JOptionPane.PLAIN_MESSAGE, null, JOptionPane.OK_CANCEL_OPTION, null, null, null, null); //$NON-NLS-1$
  }
  
  /**
   * Is the specified player allowed to joing this room?
   * @param p A JabberPlayer
   * @return true/false
   */
  public String canJoin(JabberPlayer p) {
    
    // Owner can always join
    if (isOwnedByMe()) {
      return null;
    }
    
    // Check Vassal Version
    String option = getVassalOption();
    if (!ANY_OPTION.equals(option)) {
      final String thisVassal = Info.getVersion();
      final String targetVassal = getVassalVersion();
      if (MINIMUM_OPTION.equals(option)) {
        if (Info.compareVersions(thisVassal, targetVassal) < 1) {
          return Resources.getString("Chat.bad_min_vassal", thisVassal, targetVassal); //$NON-NLS-1$
        }
      }
      else {
        if (! thisVassal.equals(targetVassal)) {
          return Resources.getString("Chat.bad_vassal", thisVassal, targetVassal); //$NON-NLS-1$
        }
      }
    }
    // Check Module Version
    option = getModuleOption();
    if (!ANY_OPTION.equals(option)) {
      final String thisModule = GameModule.getGameModule().getGameVersion();
      final String targetModule = getModuleVersion();
      if (MINIMUM_OPTION.equals(option)) {
        if (Info.compareVersions(thisModule, targetModule) < 1) {
          return Resources.getString("Chat.bad_min_module", thisModule, targetModule); //$NON-NLS-1$
        }
      }
      else {
        if (! thisModule.equals(targetModule)) {
          return Resources.getString("Chat.bad_module", thisModule, targetModule); //$NON-NLS-1$
        }
      }
    }
    
    // Check CRC
    if (isMatchCrc()) {
      final String playerCRC = ((SimpleStatus) p.getStatus()).getCrc();
      final String moduleCRC = getCheckCrc();
      if (!moduleCRC.equals(playerCRC)) {
        return Resources.getString("Chat.bad_crc"); //$NON-NLS-1$
      }
    }
    return null;
  }
  
  public static class Manager {
    private Map<String, JabberRoom> jidToRoom = new HashMap<String, JabberRoom>();

    public synchronized JabberRoom getRoomByJID(JabberClient client, String jid) {
      if (jid == null) {
        return null;
      }
      JabberRoom newRoom = jidToRoom.get(jid);
      if (newRoom == null) {
        String subject = "<no name>"; //$NON-NLS-1$
        RoomInfo info = null;
        try {
          info = MultiUserChat.getRoomInfo(client.getConnection(), jid);
          subject = info.getSubject();
        }
        // FIXME: review error message
        catch (XMPPException e) {
          e.printStackTrace();
          return null;
        }
        String roomName = ""; //$NON-NLS-1$
        try {
          roomName = new PropertiesEncoder(subject).getProperties().getProperty(CONFIG_NAME);
        }
        catch (IOException e) {
        }
        if (roomName == null) roomName = ""; //$NON-NLS-1$
        newRoom = new JabberRoom(roomName, jid, info, client);
        jidToRoom.put(jid, newRoom);
      }
      return newRoom;
    }

    public synchronized JabberRoom getRoomByName(JabberClient client,
        String name) {
      String jid = StringUtils.escapeNode(client.getModule() + "/" + name) //$NON-NLS-1$
          .toLowerCase()
          + "@" + client.getConferenceService(); //$NON-NLS-1$
      JabberRoom room = jidToRoom.get(jid);
      if (room == null) {
        room = new JabberRoom(name, jid, null, client);
        jidToRoom.put(jid, room);
      }
      return room;
    }

    public void deleteRoom(String jid) {
      jidToRoom.remove(jid);
    }

    public synchronized void clear() {
      jidToRoom.clear();
    }
  }
  

  public static Properties configureNewRoom() {
    final JabberRoomConfig config = new JabberRoomConfig();    
    int result = ((Integer) Dialogs.showDialog(null, Resources.getString("Chat.create_new_room"), config, JOptionPane.PLAIN_MESSAGE, null, JOptionPane.OK_CANCEL_OPTION, null, null, null, null)).intValue(); //$NON-NLS-1$
    if (result == 0) {  
      return config.getProperties();
    }
    return null;
  }
  
  private static final String MINIMUM_OPTION = "min"; //$NON-NLS-1$
  private static final String ANY_OPTION = "any"; //$NON-NLS-1$
  private static final String THIS_OPTION = "this"; //$NON-NLS-1$

  private static final String MINIMUM_VERSION = Resources.getString("Chat.mimimum_version"); //$NON-NLS-1$
  private static final String ANY_VERSION = Resources.getString("Chat.any_version"); //$NON-NLS-1$
  private static final String THIS_VERSION = Resources.getString("Chat.this_version"); //$NON-NLS-1$

  public static class JabberRoomConfig extends JPanel {
    private static final long serialVersionUID = 1L;
    private JTextField roomNameConfig;
    private BooleanConfigurer startLockedConfig;
    private BooleanConfigurer matchCrcConfig;
    private JTextField crcConfig;
    private StringEnumConfigurer vassalVersionConfig;
    private JTextField minimumVassalVersionConfig;
    private StringEnumConfigurer moduleVersionConfig;
    private JTextField minimumModuleVersionConfig;
    private String vassalVersion;
    private String moduleVersion;
    private boolean updateEnabled = true;
    
    static String versionToOption(String version) {
      if (MINIMUM_VERSION.equals(version)) {
        return MINIMUM_OPTION;
      }
      else if (THIS_VERSION.equals(version)) {
        return THIS_OPTION;
      }
      return ANY_OPTION; 
    }
    
    static String optionToVersion(String option) {
      if (MINIMUM_OPTION.equals(option)) {
        return MINIMUM_VERSION;
      }
      else if (THIS_OPTION.equals(option)) {
        return THIS_VERSION;
      }
      return ANY_VERSION; 
    }
    
    public JabberRoomConfig() {
      super();
      
      vassalVersion = Info.getVersion();
      moduleVersion = GameModule.getGameModule().getGameVersion();
      setLayout(new MigLayout("insets dialog", "[align right][fill,grow]", "")); //$NON-NLS-1$  //$NON-NLS-2$ //$NON-NLS-3$
      
      
      add(new JLabel(Resources.getString("Chat.new_room_name"))); //$NON-NLS-1$
      roomNameConfig = new JTextField();
      add(roomNameConfig, "wrap"); //$NON-NLS-1$
      
      add(new JLabel(Resources.getString("Chat.start_locked"))); //$NON-NLS-1$
      startLockedConfig = new BooleanConfigurer(null, ""); //$NON-NLS-1$
      add(startLockedConfig.getControls(), "wrap"); //$NON-NLS-1$
      
      add(new JLabel(Resources.getString("Chat.vassal_versions_allowed")));     //$NON-NLS-1$
      vassalVersionConfig = new StringEnumConfigurer(null, "", new String[] {ANY_VERSION, THIS_VERSION, MINIMUM_VERSION}); //$NON-NLS-1$
      vassalVersionConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateVisibility();          
        }});
      add(vassalVersionConfig.getControls());
      minimumVassalVersionConfig = new JTextField(12);
      minimumVassalVersionConfig.setText(vassalVersion);
      add(minimumVassalVersionConfig, "wrap"); //$NON-NLS-1$
      
      add(new JLabel(Resources.getString("Chat.module_versions_allowed"))); //$NON-NLS-1$
      moduleVersionConfig = new StringEnumConfigurer(null, "", new String[] {ANY_VERSION, THIS_VERSION, MINIMUM_VERSION}); //$NON-NLS-1$
      moduleVersionConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateVisibility();          
        }});
      add(moduleVersionConfig.getControls());
      minimumModuleVersionConfig = new JTextField(12);
      minimumModuleVersionConfig.setText(moduleVersion);
      add(minimumModuleVersionConfig, "wrap"); //$NON-NLS-1$
      
      add(new JLabel(Resources.getString("Chat.crc_match"))); //$NON-NLS-1$
      matchCrcConfig = new BooleanConfigurer(null, ""); //$NON-NLS-1$
      matchCrcConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateVisibility();          
        }});
      add(matchCrcConfig.getControls()); 
      crcConfig = new JTextField(12);
      crcConfig.setText(Long.toHexString(GameModule.getGameModule().getCrc()));
      crcConfig.setEnabled(false);
      add(crcConfig, "wrap"); //$NON-NLS-1$
      
      updateVisibility();
    }
    
    public JabberRoomConfig(Properties props) {
      this();
      roomNameConfig.setText(props.getProperty(CONFIG_NAME));
      startLockedConfig.setValue(new Boolean("true".equals(props.getProperty(CONFIG_LOCKED)))); //$NON-NLS-1$
      vassalVersionConfig.setValue(optionToVersion(props.getProperty(CONFIG_VASSAL_VERSION, ANY_VERSION)));
      minimumVassalVersionConfig.setText(props.getProperty(CONFIG_MIN_VASSAL_VERSION, "")); //$NON-NLS-1$
      moduleVersionConfig.setValue(optionToVersion(props.getProperty(CONFIG_MODULE_VERSION, ANY_VERSION)));
      minimumModuleVersionConfig.setText(props.getProperty(CONFIG_MIN_MODULE_VERSION, "")); //$NON-NLS-1$
      matchCrcConfig.setValue(new Boolean("true".equals(props.getProperty(CONFIG_CRC_CHECK)))); //$NON-NLS-1$
      crcConfig.setText(props.getProperty(CONFIG_CRC)); //$NON-NLS-1$
    }
    
    public JabberRoomConfig(Properties props, boolean enabled) {
      this(props);
      setEnabled(enabled);
    }
    
    
    public boolean isUpdateEnabled() {
      return updateEnabled;
    }
    
    public void setEnabled(boolean enabled) {
      updateEnabled = enabled;
      updateVisibility();
    }
    
    private void updateVisibility() {
      minimumVassalVersionConfig.setVisible(! ANY_VERSION.equals(vassalVersionConfig.getValueString()));
      minimumModuleVersionConfig.setVisible(! ANY_VERSION.equals(moduleVersionConfig.getValueString()));
      crcConfig.setVisible("true".equals(matchCrcConfig.getValueString())); //$NON-NLS-1$
      
      roomNameConfig.setEnabled(isUpdateEnabled());
      startLockedConfig.getControls().setEnabled(isUpdateEnabled());
      vassalVersionConfig.setEnabled(isUpdateEnabled());
      minimumVassalVersionConfig.setEnabled(isUpdateEnabled() && vassalVersionConfig.getValueString().equals(MINIMUM_VERSION));
      moduleVersionConfig.setEnabled(isUpdateEnabled());
      minimumModuleVersionConfig.setEnabled(isUpdateEnabled() && moduleVersionConfig.getValueString().equals(MINIMUM_VERSION));
      matchCrcConfig.getControls().setEnabled(isUpdateEnabled());
      crcConfig.setEnabled(false);
    }
    
    public Properties getProperties() {
      Properties props = new Properties();
      props.put(CONFIG_NAME, roomNameConfig.getText());
      props.put(CONFIG_LOCKED, startLockedConfig.getValueString());
      props.put(CONFIG_VASSAL_VERSION, versionToOption(vassalVersionConfig.getValueString()));
      props.put(CONFIG_MIN_VASSAL_VERSION, minimumVassalVersionConfig.getText());
      props.put(CONFIG_MODULE_VERSION, versionToOption(moduleVersionConfig.getValueString()));
      props.put(CONFIG_MIN_MODULE_VERSION, minimumModuleVersionConfig.getText());
      props.put(CONFIG_CRC_CHECK, matchCrcConfig.getValueString());
      props.put(CONFIG_CRC, crcConfig.getText());
      return props;
    }

  }
}
