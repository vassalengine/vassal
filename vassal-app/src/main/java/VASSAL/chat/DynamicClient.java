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
package VASSAL.chat;

import java.io.IOException;
import java.util.Properties;
import java.util.concurrent.ExecutionException;

import javax.swing.SwingWorker;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;

/**
 * Refreshes every time the user attempts to connect
 *
 * @author rkinney
 *
 */
public class DynamicClient extends HybridClient {
  private static final Logger log = LoggerFactory.getLogger(DynamicClient.class);

  private boolean connecting;

  protected ChatServerConnection buildDelegate() throws IOException {
    final Properties p = ServerAddressBook.getInstance().getCurrentServerProperties();
    return ChatServerFactory.build(p);
  }

  @Override
  public void setConnected(final boolean connect) {
    if (connect && !isConnected()) {
      if (!connecting) {
        connecting = true;
        new SwingWorker<ChatServerConnection, Void>() {
          @Override
          protected ChatServerConnection doInBackground() throws Exception {
            return buildDelegate();
          }

          @Override
          protected void done() {
            try {
              setDelegate(get());
              DynamicClient.super.setConnected(connect);
            }
            catch (final InterruptedException e) {
              log.error("Error while connecting: interrupted", e); //NON-NLS
            }
            catch (final ExecutionException ex) {
              final Throwable e = ex.getCause();
              fireStatus(Resources.getString("Server.bad_address3")); //$NON-NLS-1$
              ErrorDialog.showDetails(e, ThrowableUtils.getStackTrace(e), "Error.network_communication_error"); //$NON-NLS-1$
              e.printStackTrace();
            }
            connecting = false;
          }
        }.execute();
      }
    }
    else {
      super.setConnected(connect);
      if (!isConnected()) {
        try {
          setDelegate(buildDelegate());
        }
        catch (final IOException ex) {
          log.error("Error while connecting", ex); //NON-NLS
        }
      }
    }
  }
}
