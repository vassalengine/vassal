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
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Properties;

import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

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
  private static final String P2P_BUTTON = Resources.getString("Server.direct"); //$NON-NLS-1$
  private static final String OFFICIAL_BUTTON = Resources.getString("Server.official"); //$NON-NLS-1$
  private static final String ENCODING = "UTF-8"; //$NON-NLS-1$
  protected JComponent controls;
  private final HybridClient client;
  private JRadioButton officialButton;
  private JRadioButton p2pButton;
  private JLabel header;

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
      final ButtonGroup group = new ButtonGroup();

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

      officialButton = new JRadioButton(OFFICIAL_BUTTON);
      officialButton.addItemListener(e -> {
        if (e.getStateChange() == ItemEvent.SELECTED) {
          noUpdate = true;
          setValue(buildLegacyProperties());
          noUpdate = false;
        }
      });
      controls.add(officialButton);
      group.add(officialButton);
    }
    return controls;
  }

  private void enableControls(boolean connected) {
    p2pButton.setEnabled(!connected);
    officialButton.setEnabled(!connected);
    header.setText(connected ? CONNECTED : DISCONNECTED);
  }

  protected Properties buildPeerProperties() {
    final Properties p = new Properties();
    p.setProperty(ChatServerFactory.TYPE_KEY, P2PClientFactory.P2P_TYPE);
    return p;
  }

  protected Properties buildLegacyProperties() {
    final Properties p = new Properties();
    p.setProperty(ChatServerFactory.TYPE_KEY, OfficialNodeClientFactory.OFFICIAL_TYPE);
    return p;
  }

  @Override
  public String getValueString() {
    String s = ""; //$NON-NLS-1$
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    try {
      final Properties p = (Properties) getValue();
      if (p != null) {
        p.store(out, null);
      }
      s = new String(out.toByteArray(), ENCODING);
    }
    // FIXME: review error message
    catch (final IOException e) {
      e.printStackTrace();
    }
    return s;
  }

  @Override
  public void setValue(Object o) {
    super.setValue(o);
    if (!noUpdate && o instanceof Properties && controls != null) {
      final Properties p = (Properties) o;
      final String type = p.getProperty(ChatServerFactory.TYPE_KEY, OfficialNodeClientFactory.OFFICIAL_TYPE);
      if (OfficialNodeClientFactory.OFFICIAL_TYPE.equals(type)) {
        officialButton.setSelected(true);
      }
      else if (P2PClientFactory.P2P_TYPE.equals(type)) {
        p2pButton.setSelected(true);
      }
    }
    if (client != null && !CONNECTED.equals(header.getText())) {
      client.setDelegate(ChatServerFactory.build(getServerInfo()));
    }
  }

  @Override
  public void setValue(String s) {
    final Properties p = new Properties();
    try {
      p.load(new ByteArrayInputStream(s.getBytes(ENCODING)));
    }
    // FIXME: review error message
    catch (final IOException e) {
      e.printStackTrace();
    }
    setValue(p);
  }

  private Properties getServerInfo() {
    final Properties p = (Properties) getValue();
    return p == null ? new Properties() : new Properties(p);
  }

  public static void main(String[] args) {
    ChatServerFactory.register(OfficialNodeClientFactory.OFFICIAL_TYPE, new OfficialNodeClientFactory());
    ChatServerFactory.register(P2PClientFactory.P2P_TYPE, new P2PClientFactory());
    new MacOSXMenuManager();
    final HybridClient c = new HybridClient();
    final ServerConfigurer config = new ServerConfigurer("server", "server", c); //$NON-NLS-1$ //$NON-NLS-2$
    final JFrame f = new JFrame();
    f.getContentPane().add(config.getControls());
    f.pack();
    f.setVisible(true);
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
  }
}
