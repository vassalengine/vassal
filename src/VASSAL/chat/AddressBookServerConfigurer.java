/*
 * $Id$
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

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Properties;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.io.IOUtils;

/**
 * Improved version of ServerConfigurer that includes an Address Book of
 * commonly visited Jabber servers and P2P clients.
 *
 */
public class AddressBookServerConfigurer extends Configurer {
  private static final String CONNECTED = Resources.getString("Server.please_disconnect"); //$NON-NLS-1$
  private static final String DISCONNECTED = Resources.getString("ServerAddressBook.select_server"); //$NON-NLS-1$
  private static final String ENCODING = "UTF-8"; //$NON-NLS-1$
  protected JComponent controls;
  protected ServerAddressBook addressBook;
  private HybridClient client;
  private JLabel header;

  public AddressBookServerConfigurer(String key, String name, HybridClient client) {
    super(key, name, client);
    this.client = client;
    client.addPropertyChangeListener(ChatServerConnection.CONNECTED, new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        enableControls(Boolean.TRUE.equals(evt.getNewValue()));
      }
    });
    getControls();
    setValue(addressBook.getDefaultServerProperties());
    client.updateDisplayControls(addressBook.getCurrentIcon(), addressBook.getCurrentDescription());
  }

  public Component getControls() {

    if (controls == null) {
      controls = new JPanel(new MigLayout());
      header = new JLabel(DISCONNECTED);
      controls.add(header, "wrap"); //$NON-NLS-1$
      addressBook = new ServerAddressBook();
      addressBook.addPropertyChangeListener(new PropertyChangeListener(){
        public void propertyChange(PropertyChangeEvent e) {
          if (ServerAddressBook.CURRENT_SERVER.equals(e.getPropertyName())) {
            addressBook.setFrozen(true);
            setValue((Properties) e.getNewValue());
            addressBook.setFrozen(false);
          }
        }});
      controls.add(addressBook.getControls());
    }

    return controls;
  }

  private void enableControls(boolean connected) {
    addressBook.setEnabled(!connected);
    header.setText(connected ? CONNECTED : DISCONNECTED);
  }

  public void setValue(Object o) {
    super.setValue(o);
    if (!noUpdate && o instanceof Properties && controls != null) {
      addressBook.setCurrentServer((Properties) o);
    }
    if (client != null && !CONNECTED.equals(header.getText())) {
      client.setDelegate(ChatServerFactory.build(getServerInfo()));
      client.updateDisplayControls(addressBook.getCurrentIcon(), addressBook.getCurrentDescription());
    }
  }

  private Properties getServerInfo() {
    Properties p = (Properties) getValue();
    if (p == null) {
      p = new Properties();
    }
    return p;
  }

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
    catch (UnsupportedEncodingException e) {
      e.printStackTrace();
    }
    // FIXME: review error message
    catch (IOException e) {
      e.printStackTrace();
    }
    finally {
      IOUtils.closeQuietly(out);
    }
    return s;
  }

  public void setValue(String s) {
    Properties p = new Properties();
    try {
      p.load(new ByteArrayInputStream(s.getBytes(ENCODING)));
    }
    // FIXME: review error message
    catch (UnsupportedEncodingException e) {
      e.printStackTrace();
    }
    // FIXME: review error message
    catch (IOException e) {
      e.printStackTrace();
    }
    setValue(p);
  }

}
