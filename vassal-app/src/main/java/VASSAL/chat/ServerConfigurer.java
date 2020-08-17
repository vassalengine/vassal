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
package VASSAL.chat;

import java.awt.Component;
import java.awt.event.ItemEvent;
import java.beans.PropertyChangeEvent;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Properties;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.WindowConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

import net.miginfocom.swing.MigLayout;

import org.jivesoftware.smack.util.StringUtils;

import VASSAL.chat.jabber.JabberClientFactory;
import VASSAL.chat.node.OfficialNodeClientFactory;
import VASSAL.chat.peer2peer.P2PClientFactory;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.menu.MacOSXMenuManager;

/**
 * Specifies the server implementation in the Preferences
 *
 * @author rkinney
 *
 */
public class ServerConfigurer extends Configurer {
  private static final String CONNECTED = Resources.getString("Server.please_disconnect"); //$NON-NLS-1$
  private static final String DISCONNECTED = Resources.getString("Server.select_server_type"); //$NON-NLS-1$
  private static final String JABBER_BUTTON = Resources.getString("Server.jabber"); //$NON-NLS-1$
  private static final String P2P_BUTTON = Resources.getString("Server.direct"); //$NON-NLS-1$
  private static final String LEGACY_BUTTON = Resources.getString("Server.legacy"); //$NON-NLS-1$
  private static final String ENCODING = "UTF-8"; //$NON-NLS-1$
  protected JComponent controls;
  private JTextField jabberHost;
  private HybridClient client;
  private JRadioButton legacyButton;
  private JRadioButton jabberButton;
  private JTextField jabberAccountName;
  private JPasswordField jabberPassword;
  private JRadioButton p2pButton;
  private JLabel header;
  private JCheckBox jabberHostPrompt;
  private JLabel jabberAccountPrompt;
  private JLabel jabberPasswordPrompt;
  private DocumentListener docListener;

  public ServerConfigurer(String key, String name, HybridClient client) {
    super(key, name, new Properties());
    this.client = client;
    client.addPropertyChangeListener(
      ChatServerConnection.CONNECTED,
      e -> enableControls(Boolean.TRUE.equals(e.getNewValue()))
    );
    getControls();
    setValue(buildLegacyProperties());
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      controls = new JPanel(new MigLayout());
      header = new JLabel(DISCONNECTED);
      controls.add(header, "wrap"); //$NON-NLS-1$
      ButtonGroup group = new ButtonGroup();
      jabberButton = new JRadioButton(JABBER_BUTTON);
      jabberButton.addItemListener(e -> {
        if (e.getStateChange() == ItemEvent.SELECTED) {
          noUpdate = true;
          setValue(buildJabberProperties());
          noUpdate = false;
        }
        jabberHostPrompt.setEnabled(jabberButton.isSelected());
        jabberHost.setEnabled(jabberButton.isSelected() && jabberHostPrompt.isSelected());
        jabberAccountName.setEnabled(jabberButton.isSelected());
        jabberPassword.setEnabled(jabberButton.isSelected());
        jabberAccountPrompt.setEnabled(jabberButton.isSelected());
        jabberPasswordPrompt.setEnabled(jabberButton.isSelected());
      });
      jabberAccountPrompt = new JLabel(Resources.getString("Server.account_name")); //$NON-NLS-1$
      jabberAccountPrompt.setEnabled(false);
      jabberAccountName = new JTextField();
      jabberAccountName.setEnabled(false);
      jabberPasswordPrompt = new JLabel(Resources.getString("Server.password")); //$NON-NLS-1$
      jabberPasswordPrompt.setEnabled(false);
      jabberPassword = new JPasswordField();
      jabberPassword.setEnabled(false);
      jabberHostPrompt = new JCheckBox(Resources.getString("Server.host")); //$NON-NLS-1$
      jabberHostPrompt.setEnabled(false);
      jabberHost = new JTextField(18);
      jabberHost.setEnabled(false);
      jabberHostPrompt.addItemListener(e -> {
        jabberHost.setEnabled(jabberHostPrompt.isSelected() && jabberButton.isSelected());
        docListener.changedUpdate(null);
      });
      jabberHost.setText(JabberClientFactory.DEFAULT_JABBER_HOST + ":" + JabberClientFactory.DEFAULT_JABBER_PORT); //$NON-NLS-1$
      docListener = new DocumentListener() {
        @Override
        public void changedUpdate(DocumentEvent e) {
          updateValue();
        }

        private void updateValue() {
          noUpdate = true;
          setValue(buildJabberProperties());
          noUpdate = false;
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
          updateValue();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
          updateValue();
        }
      };
      ((AbstractDocument) jabberAccountName.getDocument()).setDocumentFilter(new DocumentFilter() {
        @Override
        public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
          if (text != null) {
            super.replace(fb, offset, length, StringUtils.escapeNode(text).toLowerCase(), attrs);
          }
        }

        @Override
        public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
          if (string != null) {
            super.insertString(fb, offset, StringUtils.escapeNode(string).toLowerCase(), attr);
          }
        }
      });
      jabberHost.getDocument().addDocumentListener(docListener);
      jabberAccountName.getDocument().addDocumentListener(docListener);
      jabberPassword.getDocument().addDocumentListener(docListener);
      // Disable Jabber server until next release
      if ("true".equals(System.getProperty("enableJabber"))) { //$NON-NLS-1$ //$NON-NLS-2$
        group.add(jabberButton);
        controls.add(jabberButton, "wrap"); //$NON-NLS-1$
        controls.add(jabberAccountPrompt, "gap 40"); //$NON-NLS-1$
        controls.add(jabberAccountName, "wrap, growx"); //$NON-NLS-1$
        controls.add(jabberPasswordPrompt, "gap 40"); //$NON-NLS-1$
        controls.add(jabberPassword, "wrap, growx"); //$NON-NLS-1$
        controls.add(jabberHostPrompt, "gap 40"); //$NON-NLS-1$
        controls.add(jabberHost, "wrap, growx"); //$NON-NLS-1$
      }
      p2pButton = new JRadioButton(P2P_BUTTON);
      p2pButton.addItemListener(e -> {
        if (e.getStateChange() == ItemEvent.SELECTED) {
          noUpdate = true;
          setValue(buildPeerProperties());
          noUpdate = false;
        }
      });
      group.add(p2pButton);
      controls.add(p2pButton, "wrap"); //$NON-NLS-1$
      legacyButton = new JRadioButton(LEGACY_BUTTON);
      legacyButton.addItemListener(e -> {
        if (e.getStateChange() == ItemEvent.SELECTED) {
          noUpdate = true;
          setValue(buildLegacyProperties());
          noUpdate = false;
        }
      });
      controls.add(legacyButton);
      group.add(legacyButton);
    }
    return controls;
  }

  private void enableControls(boolean connected) {
    p2pButton.setEnabled(!connected);
    legacyButton.setEnabled(!connected);
    jabberButton.setEnabled(!connected);
    jabberHostPrompt.setEnabled(!connected);
    jabberHost.setEnabled(!connected && jabberHostPrompt.isSelected() && jabberButton.isSelected());
    jabberAccountName.setEnabled(!connected && jabberButton.isSelected());
    jabberPassword.setEnabled(!connected && jabberButton.isSelected());
    header.setText(connected ? CONNECTED : DISCONNECTED);
  }

  protected Properties buildJabberProperties() {
    Properties p = new Properties();
    p.setProperty(ChatServerFactory.TYPE_KEY, JabberClientFactory.JABBER_TYPE);
    p.putAll(getJabberConfigProperties());
    return p;
  }

  protected Properties getJabberConfigProperties() {
    Properties p = new Properties();
    p.setProperty(JabberClientFactory.JABBER_LOGIN, jabberAccountName.getText());
    p.setProperty(JabberClientFactory.JABBER_PWD, new String(jabberPassword.getPassword()));
    String host = jabberHost.getText();
    String port = "5222"; //$NON-NLS-1$
    int idx = host.indexOf(':'); //$NON-NLS-1$
    if (idx > 0) {
      port = host.substring(idx + 1);
      host = host.substring(0, idx);
    }
    p.setProperty(JabberClientFactory.JABBER_HOST, host);
    p.setProperty(JabberClientFactory.JABBER_PORT, port);
    return p;
  }

  protected Properties buildPeerProperties() {
    Properties p = new Properties();
    p.setProperty(ChatServerFactory.TYPE_KEY, P2PClientFactory.P2P_TYPE);
    p.putAll(getJabberConfigProperties());
    return p;
  }

  protected Properties buildLegacyProperties() {
    Properties p = new Properties();
    p.setProperty(ChatServerFactory.TYPE_KEY, OfficialNodeClientFactory.OFFICIAL_TYPE);
    p.putAll(getJabberConfigProperties());
    return p;
  }

  @Override
  public String getValueString() {
    String s = ""; //$NON-NLS-1$
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    try {
      Properties p = (Properties) getValue();
      if (p != null) {
        p.store(out, null);
      }
      s = new String(out.toByteArray(), ENCODING);
    }
    // FIXME: review error message
    catch (IOException e) {
      e.printStackTrace();
    }
    return s;
  }

  @Override
  public void setValue(Object o) {
    super.setValue(o);
    if (!noUpdate && o instanceof Properties && controls != null) {
      Properties p = (Properties) o;
      String type = p.getProperty(ChatServerFactory.TYPE_KEY, JabberClientFactory.JABBER_TYPE);
      if (OfficialNodeClientFactory.OFFICIAL_TYPE.equals(type)) {
        legacyButton.setSelected(true);
      }
      else if (JabberClientFactory.JABBER_TYPE.equals(type)) {
        jabberButton.setSelected(true);
        jabberHostPrompt.setSelected(true);
      }
      else if (P2PClientFactory.P2P_TYPE.equals(type)) {
        p2pButton.setSelected(true);
      }
      jabberAccountName.setText(p.getProperty(JabberClientFactory.JABBER_LOGIN));
      jabberPassword.setText(p.getProperty(JabberClientFactory.JABBER_PWD));
      jabberHost.setText(p.getProperty(JabberClientFactory.JABBER_HOST, JabberClientFactory.DEFAULT_JABBER_HOST) + ":" //$NON-NLS-1$
          + p.getProperty(JabberClientFactory.JABBER_PORT, JabberClientFactory.DEFAULT_JABBER_PORT));
    }
    if (client != null && !CONNECTED.equals(header.getText())) {
      client.setDelegate(ChatServerFactory.build(getServerInfo()));
    }
  }

  @Override
  public void setValue(String s) {
    Properties p = new Properties();
    try {
      p.load(new ByteArrayInputStream(s.getBytes(ENCODING)));
    }
    // FIXME: review error message
    catch (IOException e) {
      e.printStackTrace();
    }
    setValue(p);
  }

  private Properties getServerInfo() {
    Properties p = (Properties) getValue();
    p = p == null ? new Properties() : new Properties(p);

    if (!JabberClientFactory.JABBER_TYPE.equals(p.getProperty(ChatServerFactory.TYPE_KEY))) {
      p.remove(JabberClientFactory.JABBER_HOST);
      p.remove(JabberClientFactory.JABBER_PORT);
    }

    return p;
  }

  public static void main(String[] args) {
    ChatServerFactory.register(OfficialNodeClientFactory.OFFICIAL_TYPE, new OfficialNodeClientFactory());
    ChatServerFactory.register(P2PClientFactory.P2P_TYPE, new P2PClientFactory());
    ChatServerFactory.register(JabberClientFactory.JABBER_TYPE, new JabberClientFactory());
    new MacOSXMenuManager();
    HybridClient c = new HybridClient();
    ServerConfigurer config = new ServerConfigurer("server", "server", c); //$NON-NLS-1$ //$NON-NLS-2$
    JFrame f = new JFrame();
    f.getContentPane().add(config.getControls());
    f.pack();
    f.setVisible(true);
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
  }
}
