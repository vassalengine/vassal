/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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
import java.awt.Dimension;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Properties;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import VASSAL.chat.jabber.JabberClientFactory;
import VASSAL.chat.peer2peer.P2PClientFactory;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;

/**
 * Specifies the server implementation in the Preferences
 * 
 * @author rkinney
 * 
 */
// I18n: Complete
public class ServerConfigurer extends Configurer {
  private static final String CONNECTED = Resources.getString("Server.please_disconnect");
  private static final String DISCONNECTED = Resources.getString("Server.select_server_type");
  private static final String BUTTON = "Button";
  private static final String DYNAMIC_BUTTON = Resources.getString("Server.default");
  private static final String JABBER_BUTTON = Resources.getString("Server.jabber");
  private static final String DIRECT_BUTTON = Resources.getString("Server.direct");
  private static final String ENCODING = "ISO-8859-1";
  private Box controls;
  private JTextField jabberHost;
  private HybridClient client;
  private JRadioButton dynamicButton;
  private JRadioButton jabberButton;
  private JRadioButton directButton;
  private JLabel header;
  private JLabel jabberHostPrompt;

  public ServerConfigurer(String key, String name, HybridClient client) {
    super(key, name, new Properties());
    this.client = client;
    client.addPropertyChangeListener(ChatServerConnection.CONNECTED, new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        enableControls(Boolean.TRUE.equals(evt.getNewValue()));
      }
    });
    getControls();
    setValue(buildDynamicProperties());
  }

  public Component getControls() {
    if (controls == null) {
      controls = Box.createVerticalBox();
      header = new JLabel(DISCONNECTED);
      controls.add(header);
      ButtonGroup group = new ButtonGroup();
      dynamicButton = new JRadioButton(DYNAMIC_BUTTON);
      dynamicButton.setAlignmentX(0.0f);
      dynamicButton.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent e) {
          if (e.getStateChange() == ItemEvent.SELECTED) {
            noUpdate = true;
            setValue(buildDynamicProperties());
            noUpdate = false;
          }
        }
      });
      controls.add(dynamicButton);
      group.add(dynamicButton);
      Box box = Box.createHorizontalBox();
      box.setAlignmentX(0.0f);
      jabberButton = new JRadioButton(JABBER_BUTTON);
      jabberButton.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent e) {
          if (e.getStateChange() == ItemEvent.SELECTED) {
            noUpdate = true;
            setValue(buildJabberProperties());
            noUpdate = false;
          }
        }
      });
      group.add(jabberButton);
      box.add(jabberButton);
      jabberHostPrompt = new JLabel(Resources.getString("Server.host"));
      box.add(jabberHostPrompt);
      jabberHost = new JTextField(18);
      jabberHost.setMaximumSize(new Dimension(jabberHost.getMaximumSize().width, jabberHost.getPreferredSize().height));
      jabberHost.setText(JabberClientFactory.DEFAULT_JABBER_HOST+":"+JabberClientFactory.DEFAULT_JABBER_PORT);
      jabberHost.getDocument().addDocumentListener(new DocumentListener() {
        public void changedUpdate(DocumentEvent e) {
          updateValue();
        }
        private void updateValue() {
          noUpdate = true;
          setValue(buildJabberProperties());
          noUpdate = false;
        }
        public void insertUpdate(DocumentEvent e) {
          updateValue();
        }
        public void removeUpdate(DocumentEvent e) {
          updateValue();
        }
      });
      box.add(jabberHost);
      controls.add(box);
      directButton = new JRadioButton(DIRECT_BUTTON);
      directButton.setAlignmentX(0.0f);
      directButton.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent e) {
          if (e.getStateChange() == ItemEvent.SELECTED) {
            noUpdate = true;
            setValue(buildPeerProperties());
            noUpdate = false;
          }
        }
      });
      group.add(directButton);
      controls.add(directButton);
    }
    return controls;
  }

  private void enableControls(boolean connected) {
    directButton.setEnabled(!connected);
    dynamicButton.setEnabled(!connected);
    jabberButton.setEnabled(!connected);
    jabberHostPrompt.setEnabled(!connected);
    jabberHost.setEnabled(!connected);
    header.setText(connected ? CONNECTED : DISCONNECTED);
  }

  private Properties buildJabberProperties() {
    Properties p = new Properties();
    p.setProperty(BUTTON, JABBER_BUTTON);
    String host = jabberHost.getText();
    String port = "5222";
    int idx = host.indexOf(":");
    if (idx > 0) {
      port = host.substring(idx + 1);
      host = host.substring(0, idx);
    }
    p.setProperty(ChatServerFactory.TYPE_KEY, JabberClientFactory.JABBER_SERVER_TYPE);
    p.setProperty(JabberClientFactory.JABBER_HOST, host);
    p.setProperty(JabberClientFactory.JABBER_PORT, port);
    return p;
  }

  private Properties buildPeerProperties() {
    Properties p = new Properties();
    p.setProperty(BUTTON, DIRECT_BUTTON);
    p.setProperty(ChatServerFactory.TYPE_KEY, P2PClientFactory.P2P_TYPE);
    return p;
  }

  private Properties buildDynamicProperties() {
    Properties p = new Properties();
    p.setProperty(BUTTON, DYNAMIC_BUTTON);
    p.setProperty(ChatServerFactory.TYPE_KEY, DynamicClientFactory.DYNAMIC_TYPE);
    return p;
  }

  public String getValueString() {
    String s = "";
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    try {
      getServerInfo().store(out, null);
      s = new String(out.toByteArray(), ENCODING);
    }
    catch (UnsupportedEncodingException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    return s;
  }

  public void setValue(Object o) {
    super.setValue(o);
    if (!noUpdate && o instanceof Properties && controls != null) {
      Properties p = (Properties) o;
      String type = p.getProperty(BUTTON, DYNAMIC_BUTTON);
      if (DYNAMIC_BUTTON.equals(type)) {
        dynamicButton.setSelected(true);
      }
      else if (JABBER_BUTTON.equals(type)) {
        jabberButton.setSelected(true);
        jabberHost.setText(p.getProperty(JabberClientFactory.JABBER_HOST, JabberClientFactory.DEFAULT_JABBER_HOST) + ":"
            + p.getProperty(JabberClientFactory.JABBER_PORT, JabberClientFactory.DEFAULT_JABBER_PORT));
      }
      else if (DIRECT_BUTTON.equals(type)) {
        directButton.setSelected(true);
      }
    }
    if (client != null
        && !CONNECTED.equals(header.getText())) {
      client.setDelegate(ChatServerFactory.build(getServerInfo()));
    }
  }

  public void setValue(String s) {
    Properties p = new Properties();
    try {
      p.load(new ByteArrayInputStream(s.getBytes(ENCODING)));
    }
    catch (UnsupportedEncodingException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    setValue(p);
  }

  private Properties getServerInfo() {
    Properties p = (Properties) getValue();
    if (p == null) {
      p = new Properties();
    }
    return p;
  }
}
